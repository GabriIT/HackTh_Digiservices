import type { NextPage } from 'next'
import {
    Container,
    Button,
    InputRightElement,
    InputGroup,
    Input,
    Flex,
    Divider,
    Center,

  } from '@chakra-ui/react';
  import { extendTheme } from "@chakra-ui/react";
import { Search2Icon } from '@chakra-ui/icons';
import { useState } from 'react';

type Service= {
  type: string;
  publisher: string;
  title: string;
  description: string;
  trust: number;
  price?: number;
  deadline?: number;
  badge_color:string;
  image?:string;
}


type Discoverprops ={
  services: Service[];
  filterServ: Service[];
  setfilter: (services:any) => any;
}

const SearchSection = ({services, setfilter}:Discoverprops) => {
    function handleChange(e: any){
      if (e.target.value.length > 0) {
          let filteredResults = services.filter(p => {
          return ( 
          p.title.toLowerCase().match(e.target.value.toLowerCase())
          )
          });
        setfilter(filteredResults);
      }
      else{
        setfilter(services)
      }

    }
    var handleClick = () => {}

    return (
      <Flex maxW={'3xl'} p="2rem" ml="1.5rem" alignItems="start" >
        <InputGroup size="lg">
            <Input
                onChange={handleChange}
                pr="4.5rem"  
                placeholder="Search"
                _placeholder={{ color: 'gray.600' }}
            />
            <InputRightElement  width="5.75rem" >
                <Button h="2rem" size="lg" onClick={handleClick} bg="blue.500">
                    <Search2Icon color="white" />
                </Button>
            </InputRightElement>
              
        </InputGroup>

       

     </Flex>
    );
  }
  

  export default SearchSection;