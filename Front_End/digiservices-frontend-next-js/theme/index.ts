// Using a style object
import { extendTheme } from "@chakra-ui/react"

const customtheme = extendTheme({
    components: {
      Input : {
        // 1. We can update the base styles
        baseStyle: {
          field: {
            color: "gray.600"
          },
          placeholder:{
            color: "gray.800"
          }
        },
       
        
        // 3. We can add a new visual variant
        variants: {
          
        },
      },
    },
  })
  
  export default customtheme
