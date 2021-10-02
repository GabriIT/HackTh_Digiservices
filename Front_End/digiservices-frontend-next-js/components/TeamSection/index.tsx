import {
  Box,
  Text,
  chakra,
  Flex,
  SimpleGrid,
  Heading,
  Stat,
  StatLabel,
  StatHelpText,
  StatNumber,
  useColorModeValue,
} from '@chakra-ui/react';


import {
  Avatar,
  Center,
  Stack,
  Button,
  Link,
  Badge,
} from '@chakra-ui/react';
import { ReactNode } from 'react';
import { BsPerson } from 'react-icons/bs';
import { GiTwoCoins } from 'react-icons/gi';
import { GoLocation } from 'react-icons/go';

interface StatsCardProps {
  title: string;
  helpText: string;
  image:string;
}

function StatsCard(props: StatsCardProps) {
  const { title,helpText,image } = props;
  return (
      <Box
        maxW={'320px'}
        w={'full'}
        rounded={'lg'}
        p={3}
        textAlign={'center'}>
        <Avatar
          size={'3xl'}
          src={image}
          alt={'Avatar Alt'}
          mb={0}
          pos={'relative'}
          
        />
        <Heading fontSize={'2xl'} fontFamily={'body'}>
          {title}
        </Heading>
        
        <Text
          textAlign={'center'}
          color={'gray.700'}
          px={3}>
          {helpText}
        </Text>

       
        
      </Box>
  );
}

export default function TeamSection() {
  return (
    <Box 
      id="team"
      style={{scrollMarginTop:100}}
      maxW="6xl" 
      textAlign="center" 
      pt={0} 
      px={{ base: 2, sm: 2, md: 3 }} 
      marginTop={0} 
      marginBottom={"5rem"}>

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
              Know the team 
            </Text>           
          </Heading> 
      </Box>
      <Center>
        <StatsCard
            title={'Gabriele Castaldi'}
            helpText={"Mechatronics engineer, IoT devices and Python developer. Entrepreneur with marketing experience of over 10 years. Plutus Pioneers Program second cohort"}
            image="/gabri3.png"
          />
          <StatsCard
            title={'Samuel Santos'}
            image="/samuel2.png"
            helpText={"JS developer and computer Science student, develop web applications as a hobby and for freelance works. Candidate to Plutus Pioneers Program third cohort"}
          />
          <StatsCard
            title={'Frank DelPidio'}
            image="/frank2.png"
            helpText={"Backend Software Engineer, Ruby on Rails developer, SQL database engineer, Certifited AWS Solutions Associate, Plutus Pioneers Program second cohort."}
          />
        
    </Center>
    </Box>
  );
}