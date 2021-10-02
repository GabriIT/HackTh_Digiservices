import {
  Heading,
  Avatar,
  Box,
  Center,
  Image,
  Flex,
  Text,
  Stack,
  Button,
  useColorModeValue,
} from '@chakra-ui/react';

const RANDOM_IMAGE = 'https://images.unsplash.com/photo-1500648767791-00dcc994a43e?ixlib=rb-1.2.1&q=80&fm=jpg&crop=faces&fit=crop&h=200&w=200&ixid=eyJhcHBfaWQiOjE3Nzg0fQ'

type JudgeCardProps= {
  name: string;
  description: string;
  trust: number;
  price: number;
  image?:string;
}



export default function JudgeCard(props:JudgeCardProps) {

    const IMAGE = props.image ? props.image :RANDOM_IMAGE;

    return (
   <Center py={6}>
      <Box
        maxW={'320px'}
        w={'full'}
        bg={useColorModeValue('white', 'gray.800')}
        boxShadow={'2xl'}
        rounded={'md'}
        overflow={'hidden'}>

        <Flex justify={'center'} >
          <Avatar
            size={'xl'}
            src={IMAGE }
            alt={'Author'}
            css={{
              border: '2px solid white',
            }}
          />
        </Flex>

        <Box p={6}>
          <Stack spacing={0} align={'center'} mb={5}>
            <Heading fontSize={'2xl'} fontWeight={500} fontFamily={'body'}>
              {props.name}
            </Heading>
            <Text align={"center"} color={'gray.500'}>{props.description}</Text>
          </Stack>

          <Stack direction={'row'} justify={'center'} spacing={6}>
            <Stack spacing={0} align={'center'}>
              <Text fontWeight={600}>{props.trust/1000}k  DSET{"'"}s</Text>

            </Stack>
            <Stack spacing={0} align={'center'}>
              <Text fontWeight={600}> {"$ "} {props.price/1000} {" "}hour</Text>
            </Stack>
          </Stack>

          <Button
            w={'full'}
            mt={8}
            bg={useColorModeValue('blue.500', 'blue.900')}
            color={'white'}
            rounded={'md'}
            _hover={{
              transform: 'translateY(-2px)',
              boxShadow: 'lg',
            }}>
            See More
          </Button>
        </Box>
      </Box>
    </Center>
    );
  }