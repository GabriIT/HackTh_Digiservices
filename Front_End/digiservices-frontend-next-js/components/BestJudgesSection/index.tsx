import {
  Container,
  Stack,
  Flex,
  Box,
  Heading,
  Text,
  Icon,
  createIcon,
  IconProps,
  VStack,
  Tag,
  Button,
  HStack,
} from '@chakra-ui/react';
import ChakraCarousel from "../ChacraCarrousel";
import { useEffect, useState } from 'react';
import JudgeCard from '../JudgeCard';

type Judge= {
  name: string;
  description: string;
  trust: number;
  price: number;
  image?:string;
}

type BestJudgesSectionprops ={
  judges: Judge[]
}


export default function BestJudgesSection({judges}:BestJudgesSectionprops) {

  return (
    <Container maxW={'7xl'}>
      <Stack
        align={'center'}
        spacing={{ base: 4, md: 5 }}
        py={{ base: 8, md: 5 }}
        direction={{ base: 'column', md: 'row' }}
        >
        
          <Heading
            lineHeight={1.1}
            fontWeight={600}
            fontSize={{ base: '2xl', sm: '4xl', lg: '5xl' }}>
           
            <Text >
              Popular 
            </Text>
            
            <Text color={'blue.400'}>
            {" "} Judges
            </Text>
          </Heading>             

      </Stack>
      
      <Container
        py={-8}
        px={0}
        maxW={{
          base: "100%",
          sm: "30rem",
          md: "39.75rem",
          lg: "50.5rem",
          xl: "70rem",
          xxl: "77.5rem"
        }}
      >
        <ChakraCarousel gap={3}>
          {judges.map((judge, index) => (
            <JudgeCard
            key={index.toString()+judge.name}
            name={judge.name}
            description={judge.description}
            trust={judge.trust}
            price={judge.price}
            image={judge.image}
           />
          ))}
        </ChakraCarousel>
      </Container>   

    </Container>
  );
}
