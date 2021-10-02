import { createContext,useState,ReactNode, useContext } from 'react';

type Contract = {
  judge:string;
  priceForJudge:number;
  deadline?:string;
  questions: string[];
  service:{
    type:number; //1- Contant; 2- Unique;
    title:string;
    description:string;
    plege:number; //will serve to reward an individual who has been wronged
  }

}

type UserContextData= {
    isLogged:boolean;
    name:string;
    wallet:{
      dsets:number;
      adas:number;
    }
    runningContracts: Contract[];
    endedContracts:Contract[];
    //Acusations:boolean;
    //RewardsHistory:boolean;
    
    login:(username:string,password:string) => void ;
    logout:() => void;
    
}

type UserContextProviderProps = {
    children: ReactNode;
}
/*
export const UserContext = createContext( {} as UserContextData);

export function UserContextProvider({children}:UserContextProviderProps){
    const [runningContracts, setRunningContracts] = useState([]);
    const [endedContracts, setEndedContracts] = useState([]);

    const [isLogged, setIsLogged] = useState(false);
    const [name, setName] = useState('');
    const [wallet, setWallet] = useState({dsets:0,adas:0});

    function login(username:string,password:string){
      setIsLogged(true)
      setName(username)
    }

    function logout(){
      setIsLogged(false)
    }

    
    return(
        <UserContext.Provider value={{
          espisodeList,
          isLogged,
          name:,
          wallet,
          runningContracts,
          endedContracts
          //Acusations:boolean;
          //RewardsHistory:boolean;
    
          login:(username:string,password:string) => void ;
          logout:() => void;
        }}>
             {children}
        </UserContext.Provider>
    )
}

export const useUser = () => {
  return useContext(UserContext)
}*/