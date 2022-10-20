# Earn


## Narrative

The *smart contracts* in this folder implement the distribution of rewards earned by the Sudent.


### Setup

-   The Ayllu and Teacher fungible tokens are minted.
-   Ayllu tokens (minted by administrator) are stored in Vault1.
-   Teacher tokens are minted and stored by the teacher.  (Here "teacher" has a broad meaning, and could be interpreted as "the platfrom".)
-   An Enrollment NFT is locked at the student's wallet.


### Reward process

-   The teacher, who has decided the student has completed the course and thus earned the rewards:
    -   Locks the earned Ayllu tokens in Vault 2, together with a datum pointing to the currency symbol of the student's Enrollment NFT.
    -   Sends a Teacher token to the student, with a Datum pointing to the UTxO in Vault2 that stores the rewards.
-   The student retrieves the Ayllu rewards from Vault2.  A smart contract ensures that only the wallet that stores the Enrollment NFT can collect the Ayllu rewards.

This process ensures that only the intended student can collect the rewards, that only the intended rewards can be collected by the student, and that the student has to be enrolled in the course to be able to collect the rewards.

Some sample scenarios are illustrated in file `scenarios.pdf`.  These can be reproduced by running the trace emulator, as described in *Usage* below.


## Files

-   `Validators.hs`  —  script validators and minting policies
-   `Setup.hs`  —  off-chain code implementing the assumed Setup
-   `Rewards.hs`  —  off-chain code implementing the Rewards
-   `Params.hs`  —  parameters
-   `Utils.hs`  —  miscellaneous (pure) functions
-   `Trace.hs`  —  test case(s) implemented in the *trace emulator*


## Usage

Assuming you have *Nix* installed and have cloned this repository as well as the *plutus-apps* repository.  Go to directory `plutus-apps` and execute

    [bash]$ git checkout v1.0.0-alpha1
    [bash]$ nix-shell

(This is the tag found in file `earn/cabal.project` of this repository; first time execution may take a long time.)  Inside the nix-shell, go to directory `earn`, cloned from this repository, and build the project with

    [nix-shell]$ cabal build

Next, open the *repl* with

    [nix-shell]$ cabal repl

In ghci, run the trace emulator with the command

    Prelude Trace> test

