package syssec.hcc

import java.io.File
import java.io.IOException
import java.lang.Exception
import java.util.concurrent.TimeUnit
import java.io.PrintWriter
import java.io.StringWriter

fun String.runCommand(workingDir: File = File(".")) : Boolean {
    return try{
        val proc = ProcessBuilder("/bin/bash", "-c", this)
                .directory(workingDir)
                .start()
        proc.waitFor(5, TimeUnit.MINUTES)
        if(proc.isAlive()){
            proc.destroy()
            proc.waitFor()
            error("solc error: timeout after 5 minutes")
        }
        println(proc.inputStream.bufferedReader().readText())
        println(proc.errorStream.bufferedReader().readText())
        proc.exitValue() == 0
    } catch (e: IOException){
        e.printStackTrace()
        false
    }
}

fun getCompilerVersion(version: String) : String {
    return when{
        version.startsWith("0.4") -> "0.4.26"
        version.startsWith("0.5") -> "0.5.17"
        version.startsWith("0.6") -> "0.6.12"
        version.startsWith("0.7") -> "0.7.4"
        else -> error("unsupported compiler version: $version")
    }
}

fun File.compileToJsonAST() : File {
    val path = "${this.absoluteFile.parent}/output/${this.name}_json.ast"
    val outFile = File(path)
    if(outFile.exists()){
        assert(outFile.delete())
    }

    var pragma: String
    try{
        val pragmas = this.readLines().filter() { line -> line.startsWith("pragma solidity") }.map { it.split(" ")[2] }
        pragma = if(pragmas.any { !(it.startsWith("^") || it.startsWith(">")) }) pragmas.first { !(it.startsWith("^") || it.startsWith(">")) } else pragmas[0]
    }
    catch(e: NoSuchElementException){
        error("Solidity File does not contain solidity pragma.")
    }
    var solidityPragma = pragma.split("^", ";", ">=",">", "=").filter { it -> it.isNotEmpty() && it.isNotBlank() }.joinToString("")
    solidityPragma = if(pragma.startsWith("^") || pragma.startsWith(">")) getCompilerVersion(solidityPragma) else solidityPragma
    if(!"SOLC_VERSION=$solidityPragma solc -o output --ast-compact-json ${this.absoluteFile}".runCommand(this.absoluteFile.parentFile)){
        throw Exception("solc did not finish properly.")
    }
    if(!outFile.exists()){
        error("could not write output files.")
    }
    return outFile
}

fun Exception.getStackTraceAsString() : String {
    val sw = StringWriter()
    this.printStackTrace(PrintWriter(sw))
    return sw.toString()
}

fun Error.getStackTraceAsString() : String {
    val sw = StringWriter()
    this.printStackTrace(PrintWriter(sw))
    return sw.toString()
}

fun collectGarbage() {
    log("Trigger GC...(may stop the world)", "GC.")
    Runtime.getRuntime().gc()
    log("Done. *world resumes spinning*", "GC.")
}
