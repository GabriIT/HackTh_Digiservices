import { useState } from 'react';

import {
    Box,
    FormControl,
    FormLabel,
    Input,
    RadioGroup ,
    Radio ,
    Checkbox,
    Stack,
    Link,
    Button,
    Heading,
    Center,
    Text,
    useColorModeValue,
  } from '@chakra-ui/react';
  
  export default function LoginCard() {
    const [value, setValue] = useState("1")

    return (
        <Stack spacing={1} mx={'auto'} maxW={'md'} py={4} px={3}>
          <Stack align={'center'}>
            <Heading fontSize={'5xl'}>Sign in to your account</Heading>           
          </Stack> 
          <Center>
          <RadioGroup defaultValue="2" onChange={setValue} value={value}>
              <Stack spacing={5} direction="row" >             
                  <Radio size="md" name="1" value="1" colorScheme="blue" >
                    User
                  </Radio>
                  <Radio size="md" name="2" value="2" colorScheme="blue">
                    Judge
                  </Radio>
              </Stack>
            </RadioGroup></Center>
          <Box
            rounded={'lg'}
            bg={useColorModeValue('white', 'gray.700')}
            boxShadow={'lg'}
            p={8}>
            
            <Stack spacing={4}>
              <FormControl id="email">
                <FormLabel>Email address</FormLabel>
                <Input type="email" />
              </FormControl>
              <FormControl id="password">
                <FormLabel>Password</FormLabel>
                <Input type="password" />
              </FormControl>
              <Stack spacing={10}>
                <Stack
                  direction={{ base: 'column', sm: 'row' }}
                  align={'start'}
                  justify={'space-between'}>
                  <Checkbox>Remember me</Checkbox>
                  <Link href="/forgot-password" color={'blue.400'}>Forgot password?</Link>
                </Stack>
                <Button
                  bg={'blue.400'}
                  color={'white'}
                  _hover={{
                    bg: 'blue.500',
                  }}>
                  Sign in
                </Button>
              </Stack>
            </Stack>
          </Box>
        </Stack>
    );
  }