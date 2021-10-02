import type { NextPage } from 'next'
import Head from 'next/head'
import styles from '../styles/Home.module.css'
import {
    Flex
} from '@chakra-ui/react';
import LoginCard from '../components/LoginCard';



const Login: NextPage = () => {
  return (
    <div className={styles.container}>
      <Head>
        <title>Login</title>
      </Head>

     
      <Flex
        minH={'80vh'}
        align={'center'}
        justify={'center'}>
        
        <LoginCard />

      </Flex>
    </div>
  )
}

export default Login