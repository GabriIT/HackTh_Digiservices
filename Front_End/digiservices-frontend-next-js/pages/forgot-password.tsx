import type { NextPage } from 'next'
import Head from 'next/head'
import styles from '../styles/Home.module.css'
import {
    Flex
} from '@chakra-ui/react';
import ForgotPasswordForm from '../components/ForgotPasswordCard';


const ForgotPassword: NextPage = () => {
  return (
    <div className={styles.container}>
      <Head>
        <title>Forgot Password</title>
      </Head>

     
      <Flex
        minH={'50vh'}
        align={'center'}
        justify={'center'}>
            
            <ForgotPasswordForm />

      </Flex>
    </div>
  )
}

export default ForgotPassword