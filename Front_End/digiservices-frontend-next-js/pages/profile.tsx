import type { GetStaticProps } from 'next'
import React from 'react';
import {
  Table,
  Thead,
  Tbody,
  Tr,
  Th,
  Td,
  Tabs, 
  TabList,
  Flex,
  Badge,
  TabPanels,
  Tab, 
  TabPanel
} from '@chakra-ui/react';

import UserCard from '../components/UserCard';
import { services } from './api/services';
import { accusations } from './api/accusations';
import { judges } from './api/judges';
import { InfoOutlineIcon } from '@chakra-ui/icons';
import JudgeCard from '../components/JudgeCard';

type Judge= {
  name: string;
  description: string;
  trust: number;
  price: number;
  image?:string;
  isJudge: boolean;
}

type Service= {
  id:number;
  type: string;
  publisher: string;
  title: string;
  description: string;
  trust: number;
  price?: number;
  pledge:number;
  deadline?: number;
  badge_color:string;
  status: string;
  image?:string;
  judge?: string;
}

type Discoverprops ={
  accusations: Service[],
  services: Service[],
  judge: Judge
}

const ProfilePage = ({services, judge, accusations}:Discoverprops) => {
  
  return (
    <Flex px="8" py="1" alignItems='row'>

        <UserCard name={judge.name} description={judge.description} trust={judge.trust} price={judge.price} isJudge={judge.isJudge}/>

        <Tabs variant="enclosed" colorScheme="blue" size="md" mt={6} ml={6}>

          <TabList>
            <Tab>Contracts</Tab>
            <Tab>Accusations</Tab>
            <Tab>Reviews</Tab>
          </TabList>

          <TabPanels>
            <TabPanel>

            <Table variant="striped" colorScheme="gray"  size='md' border="1px" borderColor="gray.200">
              <Thead>
                <Tr>
                  <Th>Publisher</Th>
                  <Th>Service</Th>
                  <Th>Pledge</Th>
                  <Th>Price</Th>
                  <Th>Deadline</Th>
                  <Th>Status</Th>
                  <Th></Th>
                </Tr>
              </Thead>

              <Tbody>
                {services.map((service, index) => (
                  <Tr key={service.title}>
                    <Td>{service.publisher.slice(0,10)}...</Td>
                    <Td isTruncated>{service.title}</Td>
                    <Td isNumeric>{service.pledge}</Td>
                    <Td>{service.price}</Td>
                    <Td>{service.deadline}</Td>
                    <Td>{service.status == 'Running' ? <Badge variant="outline" colorScheme="blue">{service.status}</Badge> : <Badge variant="outline" colorScheme="red">{service.status}</Badge>}</Td>
                    <Td><InfoOutlineIcon/></Td>
                  </Tr>
                  ))}
              </Tbody>

            </Table>
            </TabPanel>

            <TabPanel>

              <Table variant="striped" colorScheme="gray"  size='md' border="1px" borderColor="gray.200">
                <Thead>
                  <Tr>
                    <Th>Publisher</Th>
                    <Th>Service</Th>
                    <Th>Judge</Th>
                    <Th>Price</Th>
                    <Th>Deadline</Th>
                    <Th></Th>
                  </Tr>
                </Thead>

                <Tbody>
                  {accusations.map((accusation, index) => (
                    <Tr key={accusation.title}>
                      <Td>{accusation.publisher.slice(0,10)}...</Td>
                      <Td isTruncated>{accusation.title}</Td>
                      <Td isTruncated>{accusation.judge}</Td>
                      <Td>{accusation.price}</Td>
                      <Td>{accusation.deadline}</Td>
                      <Td><InfoOutlineIcon/></Td>
                    </Tr>
                    ))}
                </Tbody>

              </Table>

            </TabPanel>

            <TabPanel>
              <p>Reviews!</p>
            </TabPanel>

          </TabPanels>
        </Tabs>
    </Flex>
  );
};

export const getStaticProps:GetStaticProps = async () => {
  
  //const {data} =  await api("services",{});
  const data = services();
  const data_accusations = accusations();
  const data_judges = {
      "name": "Mateus Torres",
      "description": "This is the face of Alluin Moonwalker, a true adventurer among blood elves. He stands tiny among others, despite his bulky frame.",
      "trust": 15000,
      "price": 15,
      image:"https://static.generated.photos/vue-static/face-generator/landing/wall/14.jpg",
      isJudge: true
  }

  return {
    props:{
      services: data.services,
      judge: data_judges,
      accusations: data_accusations.accusations
      
    },
    revalidate:60*60*24// 24 hours
  }
}

export default ProfilePage;


