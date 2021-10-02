import { chakra, Box, Flex, useColorModeValue, HStack,Image,Text,Center } from "@chakra-ui/react";
import { StarIcon } from "@chakra-ui/icons";
import { BsStar, BsStarFill, BsStarHalf } from 'react-icons/bs';
import { ArrowBackIcon,LockIcon } from '@chakra-ui/icons'
type ServiceCardProps ={
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
    image?:string;
}

interface RatingProps {
    rating: number;
    numReviews: number;
}


function Rating({ rating, numReviews }: RatingProps) {
    return (
      <Box d="flex" alignItems="center"  >
        {Array(5)
          .fill('')
          .map((_, i) => {
            const roundedRating = Math.round(rating * 2) / 2;
            if (roundedRating - i >= 1) {
              return (
                <BsStarFill
                  key={i}
                  style={{ marginLeft: '1' }}
                  color={i < rating ? '#004aad' : 'gray.300'}
                />
              );
            }
            if (roundedRating - i === 0.5) {
              return <BsStarHalf key={i} style={{ marginLeft: '1' }} color={'#004aad'} />;
            }
            return <BsStar key={i} style={{ marginLeft: '1' }} color={'#004aad'} />;
          })}
        
      </Box>
    );
  }


export default function ServiceCard(props:ServiceCardProps)  {


  return (
     <Flex
      p={3}
      w="full"
      alignItems="center"
      justifyContent="center"
    >
      <Box
        maxW="xs"
        mx="auto"
        py={3}
        bg={useColorModeValue("white", "gray.800")}
        shadow="lg"
        rounded="lg"
      >
        <Box px={4} py={2}>
          <chakra.h1
            color={useColorModeValue("gray.800", "white")}
            fontWeight="bold"
            fontSize="1xl"
            textTransform="uppercase"
            isTruncated
          >
            {props.title}
          </chakra.h1>
          <chakra.p
            mt={1}
            fontSize="sm"
            style={{display:"block",overflow: "hidden",wordWrap:"break-word",lineHeight:"1.8em",maxHeight: "3.6em"}}

            color={"gray.600"}
          >
            {props.description}
          </chakra.p>
        </Box>

         <Center
          align="center"
          fontSize="1xl"
          fontWeight={["bold", "extrabold"]}
          lineHeight="tight"
        >
           <LockIcon mx={2} color="#42A5F5"/>

          {props.pledge}
          <chakra.span
            ml={2}
            fontSize="sm"
            fontWeight="medium"
            color={"gray.500"}
          >
            {" "}
            DSET
          </chakra.span>
        </Center>
 
        

        <Flex
          alignItems="center"
          justifyContent="space-between"
          px={4}
          py={2}

          roundedBottom="lg"
        >
          {props.price ?
            <Flex align="center"
                  fontSize="5xl"
                  fontWeight={["bold", "extrabold"]}
                  lineHeight="tight">
              <chakra.h1 color="gray.800" fontWeight="bold" fontSize="lg">
                        {props.price}
              </chakra.h1>
              <chakra.span
                ml={1}
                mt={-0.5}
                fontSize="sm"
                fontWeight="medium"
                color={"gray.500"}
              >
                {" "}
                DSET
              </chakra.span></Flex>
          :
           <chakra.h1 color="gray.800" fontWeight="bold" fontSize="lg">
                      
          </chakra.h1>
          }
          <chakra.button
             as={'a'} href={`/contract/${props.id}`}
            px={2}
            py={1}
            bg="white"
            fontSize="xs"
            color="gray.900"
            fontWeight="bold"
            rounded="lg"
            textTransform="uppercase"
            _hover={{
              bg: "gray.200",
            }}
            _focus={{
              bg: "gray.400",
            }}
          >
            See details
          </chakra.button>
        </Flex>
      </Box>
    </Flex>
  );
}