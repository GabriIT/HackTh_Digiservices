import React from 'react';

import {
    Center,
    Box,
    VStack,
    Divider,
    Heading,
    Container,
    Accordion,
    AccordionItem,
    AccordionButton,
    AccordionPanel,
    AccordionIcon,
  } from "@chakra-ui/react"

interface Item {
  key:string;
  question: string;
  answer: string;
}

interface AccordionProps {
  itens:Item[];
}


export default function AccordionComponent(props: AccordionProps) {
  const { itens } = props;
  return (
   <Accordion allowMultiple>
      {itens.map((item) => {
          return(
              <AccordionItem key={item.key}>
                  <h2>
                    <AccordionButton>
                      <Box flex="1" textAlign="left">
                          {item.question}
                      </Box>
                      <AccordionIcon />
                      </AccordionButton>
                  </h2>
                  <AccordionPanel pb={4}>
                     {item.answer}
                  </AccordionPanel>
              </AccordionItem>
          )
      })}

  </Accordion>
  );
}
