import {
  Container,Stack,Heading,Text,Divider
} from '@chakra-ui/react';
import styles from '../../styles/Home.module.css'
import ChakraCarousel from "../ChacraCarrousel";
import { useEffect, useState } from 'react';
import { GiSpellBook,GiShare,GiSpanner,GiRocketThruster } from "react-icons/gi";
import { VerticalTimeline, VerticalTimelineElement }  from 'react-vertical-timeline-component';
import 'react-vertical-timeline-component/style.min.css';


export default function RoadmapSection() {

  return (
    <Container maxW={'7xl'} marginBottom={0} paddingBottom={0} id="roadmap" style={{scrollMarginTop:120}}>
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
              Project’s Development Road Map 
            </Text>
                        
          </Heading>

      <Divider marginTop="2" marginBottom="3"/>

      <VerticalTimeline className={styles.custom_line}>
           <VerticalTimelineElement
            className={styles.custom_item}
            contentArrowStyle={{ borderRight: '7px solid  rgb(255, 255, 255)' }}
            date="September – November 2021"
            iconStyle={{ background: 'rgb(33, 150, 243)', color: '#fff' }}
            icon={<GiSpellBook />}

          >
            <h2>
             Overall Proof-of-Concept
            </h2>
            <p>
              
              Demonstration to verify that certain concepts or theories have the potential for real-world application
            </p>
          </VerticalTimelineElement>

          <VerticalTimelineElement
            className={styles.custom_item}
            contentArrowStyle={{ borderRight: '7px solid  rgb(255, 255, 255)' }}
            date="December 2021"
            iconStyle={{ background: 'rgb(33, 150, 243)', color: '#fff' }}
            icon={<GiSpanner />}
          >
            <h2>
              Techinical model and solution details
            </h2>
            <p>
                The purpose is to design, develop, and implement solutions to evaluating and selecting solutions that potentially satisfy an appropriate set of allocated requirements. 
            </p>
          </VerticalTimelineElement>


          <VerticalTimelineElement
            className={styles.custom_item}
            contentArrowStyle={{ borderRight: '7px solid rgb(255, 255, 255)' }}
            date="June 2022"
            iconStyle={{ background: 'rgb(33, 150, 243)', color: '#fff' }}
            icon={<GiShare />}
          >
            <h2>
              Prototype roll out and marketing partner selection
            </h2>
          </VerticalTimelineElement>


          <VerticalTimelineElement
            className={styles.custom_item}
            contentArrowStyle={{ borderRight: '7px solid  rgb(255, 255, 255)' }}
            date="July 2022"
            iconStyle={{ background: 'rgb(33, 150, 243)', color: '#fff' }}
            icon={<GiRocketThruster />}
          >
            <h2>
             First go-live V1,10’000 members enrollment
            </h2>
          </VerticalTimelineElement>
          


        </VerticalTimeline>

    </Container>)
}