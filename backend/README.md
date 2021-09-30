## Alice and Bob Simulation - Step By Step

### Values

Entrance Fee : 300,000 DSET
Tx Fee : 1,000 DSET

Trust : 50,000 DSET
Price : 100,000 DSET
Mediator Reward : 10,000 DSET

-----------------------------------

Alice - Wallet 1 : 1,000,000 DSET
Bob - Wallet 2 : 1,000,000 DSET
Charlie - Wallet 3 : 1,000,000 DSET

**Alice - Create Account**

*Alice pays entrance fee*
*100 SIG tokens are minted*

Alice - Wallet 1 : 700,000 DSET
Bob - Wallet 2 : 1,000,000 DSET
Charlie - Wallet 3 : 1,000,000 DSET
Account : 300,000 DSET + 100 Alice SIG

```bash
runCreateAccountExample
```

**Alice - Create Contract**

*Alice pays tx fee*
*Alice deposits trust tokens inside contract*
*1 SIG token is transfered from Alice account to contract*
*1 Contract NFT is minted and transfered to the contract*

Alice - Wallet 1 : 649,000 DSET
Bob - Wallet 2 : 1,000,000 DSET
Charlie - Wallet 3 : 1,000,000 DSET
Account : 301,000 DSET + 99 Alice SIG
Contract : 50,000 DSET + Alice SIG + NFT

```bash
runCreateContractExample
```

**Alice - Create Logic**

*1 Shame Token is minted (used to identify logic)*

Alice - Wallet 1 : 649,000 DSET
Bob - Wallet 2 : 1,000,000 DSET
Charlie - Wallet 3 : 1,000,000 DSET
Account : 301,000 DSET + 99 Alice SIG
Contract : 50,000 DSET + Alice SIG + NFT
Logic : Shame Token

```bash
runCreateLogicExample
```

**Bob - Create Account**

*Bob pays entrance fee*
*100 SIG tokens are minted*

Alice - Wallet 1 : 649,000 DSET
Bob - Wallet 2 : 700,000 DSET
Charlie - Wallet 3 : 1,000,000 DSET
Account : 601,000 DSET + 99 Alice SIG + 100 Bob SIG
Contract : 50,000 DSET + Alice SIG + NFT
Logic : Shame Token

**Bob Sign Contract**

*Bob pays tx fee*
*Bob deposits trust tokens inside contract*
*Bob deposits price inside contract if contract is of type one-time (in our case it is)*
*1 SIG token is transferred from Bob to the contract*

Alice - Wallet 1 : 649,000 DSET
Bob - Wallet 2 : 549,000 DSET
Charlie - Wallet 3 : 1,000,000 DSET
Account : 602,000 DSET + 99 Alice SIG + 99 Bob SIG
Contract : 200,000 DSET + Alice SIG + Bob SIG + NFT
Logic : Shame Token

```bash
runSignContractExample
```

**Charlie Create Account**

*Charlie pays entrance fee*
*100 SIG tokens are minted*

Alice - Wallet 1 : 649,000 DSET
Bob - Wallet 2 : 549,000 DSET
Charlie - Wallet 3 : 700,000 DSET
Account : 902,000 DSET + 99 Alice SIG + 99 Bob SIG + 100 Charlie SIG
Contract : 200,000 DSET + Alice SIG + Bob SIG + NFT
Logic : Shame Token

**Charlie Sign Contract (as mediator)**

*Charlie pays tx fee*
*Charlie deposits trust tokens inside contract*
*1 SIG token is transferred from Charlie to the contract*

Alice - Wallet 1 : 649,000 DSET
Bob - Wallet 2 : 549,000 DSET
Charlie - Wallet 3 : 649,000 DSET
Account : 903,000 DSET + 99 Alice SIG + 99 Bob SIG + 99 Charlie SIG
Contract : 250,000 DSET + Alice SIG + Bob SIG + Charlie SIG + NFT
Logic : Shame Token

**Bob Accuse Alice**

*Alice (accused) SIG token is transferred from contract to logic*
*Alice trust is transferred from contract to logic*
*Bob deposits the mediator reward inside the logic*
*Charlie (mediator) SIG token is transferred from contract to logic*
*Charlie trust is transferred from contract to logic*

Alice - Wallet 1 : 649,000 DSET
Bob - Wallet 2 : 539,000 DSET
Charlie - Wallet 3 : 649,000 DSET
Account : 903,000 DSET + 99 Alice SIG + 99 Bob SIG + 99 Charlie SIG
Contract : 150,000 DSET + Bob SIG + NFT
Logic : Alice SIG  + Charlie SIG  + 110,000 DSET + Shame Token

```bash
runAccuseExample
```

**Charlie Mediates**

*Nothing changes, only the logic datum*

Alice - Wallet 1 : 649,000 DSET
Bob - Wallet 2 : 539,000 DSET
Charlie - Wallet 3 : 649,000 DSET
Account : 903,000 DSET + 99 Alice SIG + 99 Bob SIG + 99 Charlie SIG
Contract : 150,000 DSET + Bob SIG + NFT
Logic : Alice SIG  + Charlie SIG  + 110,000 DSET+ Shame Token

```bash
runMediateExample
```

**Alice / Bob / Charlie Collects Logic**

*Consider that Alice is guilty and our contract distribution when someone is guilty is 1 : 0 (Everything to the accused and nothing to the accuser)*
…

*Bob receives the price value back from the contract*
*Bob receives Alice’s trust determined by the logic (everything)*
*Alice receives the proportion of her trust determined by the logic (nothing)*
*Account receives Alice’s SIG from the logic*
*Account receives Alice’s Shame Token*
*Charlie receives his reward*
*Contract receives Charlie’s SIG from the logic*
*Contract receives Charlie’s trust back*

Alice - Wallet 1 : 649,000 DSET
Bob - Wallet 2 : 689,000 DSET
Charlie - Wallet 3 : 659,000 DSET
Account : 903,000 DSET + 100 Alice SIG + 99 Bob SIG + 99 Charlie SIG + Alice Shame Token
Contract : 100,000 DSET + Bob SIG + Charlie SIG + NFT

```bash
runLogicCollectExample
```

**Bob Leaves Contract**

*Bob receives his trust value back from the contract*
*Bob pays tx fee*
*Account receives Bob SIG from the contract*

Alice - Wallet 1 : 649,000 DSET
Bob - Wallet 2 : 738,000 DSET
Charlie - Wallet 3 : 659,000 DSET
Account : 904,000 DSET + 100 Alice SIG + 100 Bob SIG + 99 Charlie SIG + Alice Shame Token
Contract : 50,000 DSET  + Charlie SIG + NFT

**Charlie Leaves Contract**

*Charlie receives his trust value back from the contract*
*Charlie pays tx fee*
*Account receives Charlie SIG from the contract*

Alice - Wallet 1 : 649,000 DSET
Bob - Wallet 2 : 738,000 DSET
Charlie - Wallet 3 : 708,000 DSET
Account : 905,000 DSET + 100 Alice SIG + 100 Bob SIG + 100 Charlie SIG + Alice Shame Token
Contract : NFT

```bash
runLeaveContractExample
```