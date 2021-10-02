import {
    Divider,
    Heading,
    Container,
    Text,
  } from "@chakra-ui/react"

import AccordionComponent from '../AccordionComponent'

export default function FaqSection() {
  const faq  = [
        {
            key:"A",
            question:"How shall I offer a service?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"B",
            question:"How shall I sign a services?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"C",
            question:"How shall I raise an accusation issue?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"D",
            question:"How does I assess reliability of publisher or client through CAS?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"E",
            question:"What are TrustToken, as collateral?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"F",
            question:"What I can use DSET for?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"G",
            question:"Where can I purchase DSET?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"H",
            question:"What is Accumulated Review Credit?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"I",
            question:"Why is DigiServices a trustful platform?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"J",
            question:"How can I become a judge?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"K",
            question:"What is judge compensation?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"L",
            question:" What services can be offered on DigiServices?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"M",
            question:"What is EUTxO?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"N",
            question:"What is Cardano?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"O",
            question:"What is the relationship DSET with ADA?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        {
            key:"P",
            question:"What are penalties?",
            answer:"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod"
        },
        
    ]

  return (
    <Container maxW={'6xl'} marginBottom={10} id="faq" style={{scrollMarginTop:120}}>
     <Heading
        lineHeight={1.1}
        fontWeight={600}
        textAlign={'center'} 
        fontSize={{ base: '3xl', sm: '4xl', lg: '5xl' }}>
        <Text
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
          Frequently Asked Questions (FAQ)
        </Text>
                    
      </Heading>

      <Divider marginTop="5" />
      
      <AccordionComponent itens={faq}/>

    </Container>
    
  );
}