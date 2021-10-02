import React from "react";
import {
  chakra,
  Box,
  Flex,
  NumberInput,
  NumberInputStepper,
  NumberInputField,
  NumberIncrementStepper ,
  NumberDecrementStepper,
  useColorModeValue,
  SimpleGrid,
  GridItem,
  Heading,
  Text,
  Stack,
  FormControl,
  FormLabel,
  Input,
  InputGroup,
  InputLeftAddon,
  FormHelperText,
  Textarea,
  Avatar,
  Icon,
  Button,
  VisuallyHidden,
  Select,
  Checkbox,
  RadioGroup,
  Radio,
} from "@chakra-ui/react";
import { AddIcon } from '@chakra-ui/icons';
import { FaUser } from "react-icons/fa";
import { useState } from 'react';

export default function Component() {
  const [questions, setQuestions] = useState([""])

  const createNewQuestion = (e:any) => {
    setQuestions([...questions, "new"])
  }

  return (
    <Box bg={useColorModeValue("gray.50", "inherit")} p={10}>
      <Heading fontSize="lg" fontWeight="medium" lineHeight="6">
       Create a Contract
      </Heading>
      <Box visibility={{ base: "hidden", sm: "visible" }} aria-hidden="true">
        <Box py={5}>
          <Box
            borderTop="solid 1px"
            borderTopColor={useColorModeValue("gray.200", "whiteAlpha.200")}
          ></Box>
        </Box>
      </Box>

      <Box mt={[10, 0]}>
        <SimpleGrid
          display={{ base: "initial", md: "grid" }}
          columns={{ md: 3 }}
          spacing={{ md: 6 }}
        >
          <GridItem mt={[5, null, 0]} colSpan={{ md: 2 }}>
            <chakra.form

              method="POST"
              shadow="base"
              rounded={[null, "md"]}
              overflow={{ sm: "hidden" }}
            >
              <Stack
                px={4}
                py={5}
                p={[null, 6]}
                bg={useColorModeValue("white", "gray.700")}
                spacing={6}
              >
                <SimpleGrid columns={2} spacing={6}>
                  <FormControl as={GridItem} colSpan={[6, 0]}>
                    <FormLabel
                      htmlFor="service_title"
                      fontSize="sm"
                      fontWeight="md"
                      color={useColorModeValue("gray.700", "gray.50")}
                    >
                      Service Title
                    </FormLabel>
                    <Input
                      type="text"
                      name="service_title"
                      id="service_title"
                      autoComplete="family-name"
                      mt={1}
                      focusBorderColor="brand.400"
                      shadow="sm"
                      size="sm"
                      w="full"
                      rounded="md"
                    />
                  </FormControl> 
                </SimpleGrid>
                <SimpleGrid columns={1} spacing={6}>
                <div>
                  <FormControl id="email" mt={1} colSpan={[6, 1]} >
                    <FormLabel
                      fontSize="sm"
                      fontWeight="md"
                      color={useColorModeValue("gray.700", "gray.50")}
                    >
                      Service Description
                    </FormLabel>
                    <Textarea
                      placeholder="Brief description for your service"
                      mt={1}
                      rows={3}
                      w="full"
                      shadow="sm"
                      focusBorderColor="brand.400"
                      fontSize={{ sm: "sm" }}
                    />
                    
                  </FormControl>
                </div>
                </SimpleGrid>
                <SimpleGrid columns={2} spacing={6}>
                  <FormControl as={GridItem} colSpan={[6, 1]}>
                    <FormLabel
                      htmlFor="price"
                      fontSize="sm"
                      fontWeight="md"
                      color={useColorModeValue("gray.700", "gray.50")}
                    >
                      Price 
                    </FormLabel>
                    <NumberInput step={0.1}>
                      <NumberInputField />
                      <NumberInputStepper>
                        <NumberIncrementStepper />
                        <NumberDecrementStepper />
                      </NumberInputStepper>
                    </NumberInput>
                  </FormControl>
                  <FormControl as={GridItem} colSpan={[6, 1]}>
                    <FormLabel
                      htmlFor="trust_token"
                      fontSize="sm"
                      fontWeight="md"
                      color={useColorModeValue("gray.700", "gray.50")}
                    >
                      Trust Token 
                    </FormLabel>
                    <NumberInput  step={0.1}>
                      <NumberInputField />
                      <NumberInputStepper>
                        <NumberIncrementStepper />
                        <NumberDecrementStepper />
                      </NumberInputStepper>
                    </NumberInput>
                    <FormHelperText>
                     It will be used to reward an individual who has been wronged
                    </FormHelperText>
                  </FormControl>
                  <FormControl as={GridItem} colSpan={[6, 0]}>
                    <FormLabel
                      htmlFor="country"
                      fontSize="sm"
                      fontWeight="md"
                      color={useColorModeValue("gray.700", "gray.50")}
                    >
                      Type of Service
                    </FormLabel>
                   <Select variant="outline" placeholder="Outline" >
                      <option>ONE-TIME</option>
                      <option>CONSTANT</option>
                      
                    </Select>
                  </FormControl>
                </SimpleGrid>
                <SimpleGrid columns={1} spacing={6}>
                <FormControl as={GridItem} colSpan={[6, 1]}>
                    <FormLabel
                      htmlFor="service_title"
                      fontSize="sm"
                      alignContent="left"
                      fontWeight="md"
                      color={useColorModeValue("gray.700", "gray.50")}
                    >
                      Questions
                    </FormLabel>
                    {questions.map( (index,question) => (
                      <div key={index+question}>
                        <Input
                      type="text"
                      name="service_title"
                      id="service_title"
                      autoComplete="family-name"
                      mt={1}
                      focusBorderColor="brand.400"
                      shadow="sm"
                      size="md"
                      w="md"
                      rounded="md"
                    />
                    <Button
                      ml="1rem"
                      mb="0.5rem"
                      bg="blue.500"
                      colorScheme="brand"
                      fontWeight="sm"
                      size="sm"
                      _hover={{
                        bg: 'blue.400',
                      }}
                      onClick={createNewQuestion}
                    >
                      <AddIcon/>
                    </Button>


                      </div>

                    ))}
                  </FormControl> 
                  </SimpleGrid>
              </Stack>
              <Box
                px={{ base: 4, sm: 6 }}
                py={3}
                bg={"white"}
                textAlign="right"
              >
                <Button
                  type="submit"
                  bg="blue.500"
                  colorScheme="brand"
                  _focus={{ shadow: "" }}
                  fontWeight="md"
                  _hover={{
                    bg: 'blue.400',
                  }}
                >
                  Save
                </Button>
              </Box>
            </chakra.form>
          </GridItem>
        </SimpleGrid>
      </Box>

    </Box>
  );
}