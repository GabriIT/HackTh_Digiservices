## Alice and Bob Simulation - Step By Step

### Values

- Entrance Fee : 300,000 DSET
- Tx Fee : 1,000 DSET

- Trust : 50,000 DSET
- Price : 100,000 DSET
- Mediator Reward : 10,000 DSET

-----------------------------------

1. Alice - Wallet 1 : 1,000,000 DSET
2. Bob - Wallet 2 : 1,000,000 DSET
3. Charlie - Wallet 3 : 1,000,000 DSET

### Step 1: Alice - Create Account

- *Alice pays entrance fee*
- *100 SIG tokens are minted*

1. Alice - Wallet 1 : 700,000 DSET
2. Bob - Wallet 2 : 1,000,000 DSET
3. Charlie - Wallet 3 : 1,000,000 DSET
4. Account : 300,000 DSET + 100 Alice SIG

```bash
runCreateAccountExample
```

### Setp 2: Alice - Create Contract

- *Alice pays tx fee*
- *Alice deposits trust tokens inside contract*
- *1 SIG token is transfered from Alice account to contract*
- *1 Contract NFT is minted and transfered to the contract*

1. Alice - Wallet 1 : 649,000 DSET
2. Bob - Wallet 2 : 1,000,000 DSET
3. Charlie - Wallet 3 : 1,000,000 DSET
4. Account : 301,000 DSET + 99 Alice SIG
5. Contract : 50,000 DSET + Alice SIG + NFT

```bash
runCreateContractExample
```

### Step 3 : Alice - Create Logic

- *1 Shame Token is minted (used to identify logic)*

1. Alice - Wallet 1 : 649,000 DSET
2. Bob - Wallet 2 : 1,000,000 DSET
3. Charlie - Wallet 3 : 1,000,000 DSET
4. Account : 301,000 DSET + 99 Alice SIG
5. Contract : 50,000 DSET + Alice SIG + NFT
6. Logic : Shame Token

```bash
runCreateLogicExample
```

### Step 4: Bob - Create Account

- *Bob pays entrance fee*
- *100 SIG tokens are minted*

1. Alice - Wallet 1 : 649,000 DSET
2. Bob - Wallet 2 : 700,000 DSET
3. Charlie - Wallet 3 : 1,000,000 DSET
4. Account : 601,000 DSET + 99 Alice SIG + 100 Bob SIG
5. Contract : 50,000 DSET + Alice SIG + NFT
6. Logic : Shame Token

### Step 5: Bob Sign Contract

- *Bob pays tx fee*
- *Bob deposits trust tokens inside contract*
- *Bob deposits price inside contract if contract is of type one-time (in our case it is)*
- *1 SIG token is transferred from Bob to the contract*

1. Alice - Wallet 1 : 649,000 DSET
2. Bob - Wallet 2 : 549,000 DSET
3. Charlie - Wallet 3 : 1,000,000 DSET
4. Account : 602,000 DSET + 99 Alice SIG + 99 Bob SIG
5. Contract : 200,000 DSET + Alice SIG + Bob SIG + NFT
6. Logic : Shame Token

```bash
runSignContractExample
```

### Step 6: Charlie Create Account

- *Charlie pays entrance fee*
- *100 SIG tokens are minted*

1. Alice - Wallet 1 : 649,000 DSET
2. Bob - Wallet 2 : 549,000 DSET
3. Charlie - Wallet 3 : 700,000 DSET
4. Account : 902,000 DSET + 99 Alice SIG + 99 Bob SIG + 100 Charlie SIG
5. Contract : 200,000 DSET + Alice SIG + Bob SIG + NFT
6. Logic : Shame Token

### Step 7: Charlie Sign Contract (as mediator)

- *Charlie pays tx fee*
- *Charlie deposits trust tokens inside contract*
- *1 SIG token is transferred from Charlie to the contract*

1. Alice - Wallet 1 : 649,000 DSET
2. Bob - Wallet 2 : 549,000 DSET
3. Charlie - Wallet 3 : 649,000 DSET
4. Account : 903,000 DSET + 99 Alice SIG + 99 Bob SIG + 99 Charlie SIG
5. Contract : 250,000 DSET + Alice SIG + Bob SIG + Charlie SIG + NFT
6. Logic : Shame Token

### Step 8: Bob Accuse Alice

- *Alice (accused) SIG token is transferred from contract to logic*
- *Alice trust is transferred from contract to logic*
- *Bob deposits the mediator reward inside the logic*
- *Charlie (mediator) SIG token is transferred from contract to logic*
- *Charlie trust is transferred from contract to logic*

1. Alice - Wallet 1 : 649,000 DSET
2. Bob - Wallet 2 : 539,000 DSET
3. Charlie - Wallet 3 : 649,000 DSET
4. Account : 903,000 DSET + 99 Alice SIG + 99 Bob SIG + 99 Charlie SIG
5. Contract : 150,000 DSET + Bob SIG + NFT
6. Logic : Alice SIG  + Charlie SIG  + 110,000 DSET + Shame Token

```bash
runAccuseExample
```

### Step 9: Charlie Mediates

- *Nothing changes, only the logic datum*

1. Alice - Wallet 1 : 649,000 DSET
2. Bob - Wallet 2 : 539,000 DSET
3. Charlie - Wallet 3 : 649,000 DSET
4. Account : 903,000 DSET + 99 Alice SIG + 99 Bob SIG + 99 Charlie SIG
5. Contract : 150,000 DSET + Bob SIG + NFT
6. Logic : Alice SIG  + Charlie SIG  + 110,000 DSET+ Shame Token

```bash
runMediateExample
```

### Step 10: Alice / Bob / Charlie Collects Logic

- *Consider that Alice is guilty and our contract distribution when someone is guilty is 1 : 0 (Everything to the accused and nothing - to the accuser)*
- …
- 
- *Bob receives the price value back from the contract*
- *Bob receives Alice’s trust determined by the logic (everything)*
- *Alice receives the proportion of her trust determined by the logic (nothing)*
- *Account receives Alice’s SIG from the logic*
- *Account receives Alice’s Shame Token*
- *Charlie receives his reward*
- *Contract receives Charlie’s SIG from the logic*
- *Contract receives Charlie’s trust back*

1. Alice - Wallet 1 : 649,000 DSET
2. Bob - Wallet 2 : 689,000 DSET
3. Charlie - Wallet 3 : 659,000 DSET
4. Account : 903,000 DSET + 100 Alice SIG + 99 Bob SIG + 99 Charlie SIG + Alice Shame Token
5. Contract : 100,000 DSET + Bob SIG + Charlie SIG + NFT

```bash
runLogicCollectExample
```

### Step 11: Bob Leaves Contract

- *Bob receives his trust value back from the contract*
- *Bob pays tx fee*
- *Account receives Bob SIG from the contract*

1. Alice - Wallet 1 : 649,000 DSET
2. Bob - Wallet 2 : 738,000 DSET
3. Charlie - Wallet 3 : 659,000 DSET
4. Account : 904,000 DSET + 100 Alice SIG + 100 Bob SIG + 99 Charlie SIG + Alice Shame Token
5. Contract : 50,000 DSET  + Charlie SIG + NFT

### Step 12: Charlie Leaves Contract

- *Charlie receives his trust value back from the contract*
- *Charlie pays tx fee*
- *Account receives Charlie SIG from the contract*

1. Alice - Wallet 1 : 649,000 DSET
2. Bob - Wallet 2 : 738,000 DSET
3. Charlie - Wallet 3 : 708,000 DSET
4. Account : 905,000 DSET + 100 Alice SIG + 100 Bob SIG + 100 Charlie SIG + Alice Shame Token
5. Contract : NFT

```bash
runLeaveContractExample
```