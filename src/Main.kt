import java.io.File
import javax.naming.InvalidNameException
import kotlin.system.exitProcess
import org.neo4j.configuration.GraphDatabaseSettings.DEFAULT_DATABASE_NAME
import org.neo4j.configuration.connectors.BoltConnector
import org.neo4j.configuration.helpers.SocketAddress
import org.neo4j.dbms.api.DatabaseManagementServiceBuilder
import syssec.hcc.*

fun getFilenameArgument(args: Array<String>): String {
    val filename = args.findLast { it.contains(".sol") }
    if (filename.isNullOrBlank()) error("please provide a contract filename")
    return filename
}

fun getDbFolder(args: Array<String>) : String {
    val dbName = args.findLast { !it.contains(".sol") && !it.startsWith("--")}
    if(dbName.isNullOrBlank()) error("please provide a database name")
    return dbName
}

fun patchArgs(args: Array<String>) {
    if(args.contains("--re")) Mappings.patchRE = true
    if(args.contains("--io")) Mappings.patchIO = true
}

fun getOutputFilename(n: String): String {
    val segments = n.split(".")
    val nameparts = segments.subList(0, segments.lastIndex)
    val name = nameparts.joinToString(separator = ".")
    return "${name}_patched.sol"
}

fun main(args: Array<String>) {
    /*
    WebUI is enabled for use via neo4j-browser (or neo4j-desktop)
    username: neo4j
    password: neo4j
    host: localhost:7687
    no encryption
    */
    patchArgs(args)
    var dbName: String
    try {
        dbName = getDbFolder(args)
    } catch (e: Error) {
        log(e.getStackTraceAsString(), "ERROR")
        exitProcess(0)
    } catch (e: Exception) {
        log(e.getStackTraceAsString(), "ERROR")
        exitProcess(0)
    }

    val managementService = DatabaseManagementServiceBuilder(File(dbName)).build()
    try {
        val graphDb = managementService?.database(DEFAULT_DATABASE_NAME)
        Runtime.getRuntime().addShutdownHook(object : Thread() {
            override fun run(): Unit = run {
                managementService?.shutdown()
            }
        })
        if (graphDb == null) return
        val file = getFilenameArgument(args)

        log("Opening file $file")
        val srcFile = SourceFile(file)
        srcFile.loadToCPG(graphDb)
        log("Parsing finished.")
        if(Mappings.patchRE){
            cpgEnrichmentPass(graphDb)
            log("Enrichment finished.")
        }else{
            log("RE detection is disabled, skipping the Enrichment.", "BOOST")
        }
        if(Mappings.patchIO || Mappings.patchRE){
            securityAnalysisPass(graphDb)
            log("Security Analysis finished.")
            patchingPass(graphDb)
            log("Patching finished.")
        }
        val outputFile = getOutputFilename(file)
        log("Emitting patched solidity code to $outputFile.")
        try {
            emitCode(graphDb, outputFile)
        } catch (e: Error) {
            log(e.getStackTraceAsString(), "ERROR")
        } catch (e: Exception) {
            log(e.getStackTraceAsString(), "ERROR")
        }

        log("Shutting down embedded database...")
        managementService.shutdown()
        log("Finished")
        exitProcess(0)
    } catch (e: Error) {
        log(e.getStackTraceAsString(), "ERROR")
    } catch (e: Exception) {
        log(e.getStackTraceAsString(), "ERROR")
    }
    finally {
        managementService.shutdown()
    }
    System.exit(0)
}
