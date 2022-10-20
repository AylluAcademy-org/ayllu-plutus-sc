# Enrollment


## Narrative

The *smart contracts* in this folder implement the following narrative:

-   The student(s) send the tuition fee to the "registration script".
-   Each student that sent the exact tuition amount (plus *minAda*) receives an **enrollment token** (NFT), delivered to the student's wallet.
-   The tuition funds are sent to the Registrar's wallet.

The student provides the *return address*, where the enrollment token is to be delivered, in the same transaction where the tuition payment is made.  Since he/she may provide any address, this could also be a gift or part of a batch process.

Only the Registrar's wallet is authorized to retrieve funds from the registration script.

Note: the Registrar's endpoints contain the function "`logU`" that allows to log the UTxO's present at the registration script (or any other address) at any step in the process.


## Files

-   `Validators.hs`  —  script validators
-   `Policies.hs`  —  minting policies
-   `Registrar.hs`  —  off-chain code called by the Registrar's endpoint
-   `Student.hs`  —  off-chain code called by the student's endpoint
-   `Utils.hs`  —  miscellaneous (pure) functions
-   `Trace.hs`  —  test case(s) implemented in the *trace emulator*


## Usage

Assuming you have *Nix* installed and have cloned this repository as well as the *plutus-apps* repository.  Go to directory `plutus-apps` and execute

    [bash]$ git checkout 62efdd2bfab3e076d40e07f8f4d7864a7f2ccc91
    [bash]$ nix-shell

(This is the tag found in file `enrollment/cabal.project` of this repository; first time execution may take a long time.)  Inside the nix-shell, go to directory `enrollment`, cloned from this repository, and build the project with

    [nix-shell]$ cabal build

Next, open the *repl* with

    [nix-shell]$ cabal repl

In ghci, run the trace emulator with the command

    Prelude Trace> test

Some scenarios that can be tested with appropriate modifications in file `Trace.hs` are:

-   Try to withdraw from the registration script from a non-authorized address.
-   Pay the tuition with the incorrect amount.
-   No payments at all.

