import type { NextPage } from 'next'
import Head from 'next/head'
import styles from '../styles/Home.module.css'
import {
    Flex
} from '@chakra-ui/react';
import ResetPasswordForm from '../components/ForgotPasswordCard';


const ResetPassword: NextPage = () => {
  return (
    <div className={styles.container}>
      <Head>
        <title>Forgot Password</title>
      </Head>

     
      <Flex
        minH={'50vh'}
        align={'center'}
        justify={'center'}>
            
            <ResetPasswordForm />

      </Flex>
    </div>
  )
}

export default ResetPassword