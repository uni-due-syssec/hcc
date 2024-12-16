# HCC - The Hardening Contract Compiler

HCC is a new hardening compiler for smart contracts.
HCC takes the source code of a smart contract and parses it into a code property graph (CPG) to facilitate security analysis and insertion of runtime checks.

This repository contains the source code for our implementation of HCC, which targets the Solidity programming language.

# Usage

We recommend building and using the docker image:

``` shell
# clone it!
git clone https://github.com/uni-due-syssec/hcc.git

# build it!
cd hcc
docker built -t hcc .

# execute it!
docker run --rm -v /path/to/contract/folder:/contract hcc "some_db_name --re --io /contract/<filename>"
```

The last command 

- starts a new hcc container (which gets removed after execution thanks to `--rm`)
- mounts the directory `/path/to/contract/folder` as `/contract` inside the container
- provides `some_db_name` as a name for the database
- enables reentrancy hardening (`--re`)
- enables integer bug hardening (`--io`)
- tells HCC to analyze the contract in `/conract/<filename>`.

After execution, you will finde a file called `<filename w/o extension>_patched.sol` in the folder you mounted in the container.

# The HCC Paper
We describe HCC's concept, architecture, implementation and evaluation results in our paper ([Arxiv Preprint](https://arxiv.org/abs/2203.00364)), which will be published at ACNS in June 2025.

Until then, you may cite this work as follows:

```
@misc{hcc2024-preprint,
      title={HCC: A Language-Independent Hardening Contract Compiler for Smart Contracts}, 
      author={Jens-Rene Giesen and Sebastien Andreina and Michael Rodler and Ghassan O. Karame and Lucas Davi},
      year={2024},
      eprint={2203.00364},
      archivePrefix={arXiv},
      primaryClass={cs.CR},
      url={https://arxiv.org/abs/2203.00364}, 
}
```


# Evaluation Results

In our paper we describe the extensive evaluation of HCC on over 10k real-world smart contracts.
Unfortunately, the results take to much disk space to publish on via git.
However, you find the evaluation dataset and results on [here](https://figshare.com/articles/dataset/HCC_Evaluation_Dataset_and_Results/25715565?file=45988767) on figshare.
Once the paper is fully published, we will update the metadata on figshare. *Please be aware* that this could happen at any time from now.
