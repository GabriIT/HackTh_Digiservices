import {
  Box,
  Button,
  chakra,
  Heading,
  Stack,
  Flex,
  Center,
  SimpleGrid,
  Text,
  UnorderedList,
  ListItem ,
  useColorModeValue,
} from "@chakra-ui/react";

import { GiCoins,GiHandOk,GiChart } from "react-icons/gi";

export default function TokenSection() {
  return (
    <Box   >
    
    <Flex
      p={16}
      paddingBottom={0}
      style={{textAlign:"center",scrollMarginTop:50}} id="token" pt={0}
      w="full"
      justifyContent="center"
      alignItems="center"
    >
      <Box   
        px={8}
        py={20}
        mx="auto"
      >
          <Box >
          <Heading
      fontWeight={600}
      paddingBottom={5}
      fontSize={{ base: '3xl', sm: '4xl', lg: '5xl' }}>
      <Text
      paddingBottom={0}
        as={'span'}
        position={'relative'}
        _after={{
          content: "''",
          width: 'full',
          height: '30%',
          position: 'absolute',
          bottom: 1,
          left: 0,
          bg: 'blue.400',
          zIndex: -1,
        }}>
        Tokenomics 
      </Text>
                  
    </Heading> 
          </Box>
          <Box order={{ base: 0, md: 2 }}>            
            <chakra.p
              mb={1}
              textAlign={{ base: "justify", sm: "justify" }}
              color={useColorModeValue("gray.600", "gray.400")}
              fontSize={{ md: "lg" }}
            >
            Differently from traditional currencies, such as Bitcoin, Litecoin and Ethereum, DigiServices tokens (DSET) are not deflationary. This is important in order to incentivise cooperative and honest behaviour in the platform. A fixed amount of DSET tokens is monthly minted and distributed according to a Credit Assessment System (CAS). 
            Users receive tokens proportionally to their scores. The token will have a total supply of 1 billion distributed as follows:
            </chakra.p>

            
          <UnorderedList paddingLeft="3rem">
              <ListItem fontSize="lg" textAlign={"left"}> 10% - Institutional investor</ListItem>
              <ListItem fontSize="lg" textAlign={"left"}> 20% - Core Team {"&"} Marketing and Developement Partner</ListItem>
              <ListItem fontSize="lg" textAlign={"left"}> 70% - Public Sale</ListItem>
          </UnorderedList>

            <chakra.p
              mb={5}
              textAlign={{ base: "justify", sm: "justify" }}
              color={useColorModeValue("gray.600", "gray.400")}
              fontSize={{ md: "lg" }}
            >
            The membership rewarding fund will be supported by 5% of transaction fees. In this way we assure the totally issued DSET tokens amount will be capped at 1 billion units.
            </chakra.p>

            
          </Box>
        <SimpleGrid
          alignItems="start"
          columns={{ base: 1, md: 2 }}
          mb={0}
          spacingY={{ base: 10, md: 32 }}
          spacingX={{ base: 10, md: 24 }}
        >
          <Box>
            <chakra.h2
              mb={4}
              fontSize={{ base: "2xl", md: "4xl" }}
              fontWeight="extrabold"
              letterSpacing="tight"
              textAlign={{ base: "center", md: "left" }}
              color={useColorModeValue("gray.900", "gray.400")}
              lineHeight={{ md: "shorter" }}
              textShadow="2px 0 currentcolor"
            >
              Utility
            </chakra.h2>
            <chakra.p
              mb={5}
              letterSpacing="tight"
              textAlign={{ base: "justify", sm: "justify" }}
              color={useColorModeValue("gray.600", "gray.400")}
              fontSize={{ md: "lg" }}
            >
              In order to de-incentive dishonest behaviour, DigiServices makes it possible for service providers to lock an arbitrary amount of tokens (Trust Token, TT) into the contract proposal in the marketplace and request from the client the same amount. 
              In this way, there is commitment by both parties and a pledge in case of a conflict.
            </chakra.p>

             <chakra.p
              mb={5}
              letterSpacing="tight"
              textAlign={{ base: "justify", sm: "justify" }}
              color={useColorModeValue("gray.600", "gray.400")}
              fontSize={{ md: "lg" }}
            >
              Thus DSET utility provides the basis for a trustworthy platform by setting a good measure of reliability through the CAS (Credit Assessment System);furthermore it is an important element for the decentralized voting mechanism about the platform development.
            </chakra.p>
           
          </Box>

          <Box order={{ base: 0, md: 2 }}>
            <chakra.h2
              mb={4}
              fontSize={{ base: "2xl", md: "4xl" }}
              fontWeight="extrabold"
              letterSpacing="tight"
              textAlign={{ base: "center", md: "left" }}
              color={useColorModeValue("gray.900", "gray.400")}
              lineHeight={{ md: "shorter" }}
            >
              Network
            </chakra.h2>
            <chakra.p
              mb={0}
              letterSpacing="tight"
              textAlign={{ base: "justify", sm: "justify" }}
              color={useColorModeValue("gray.600", "gray.400")}
              fontSize={{ md: "lg" }}
            >
              User{"'"}s activiy is rewarded: inviting new members, acting as a judge, scoring a high CAS provide benefit for the member and for the platform as a whole.
            </chakra.p>
            
          </Box>
         
        </SimpleGrid>
        
      </Box>
    </Flex>
    </Box>
  );
}