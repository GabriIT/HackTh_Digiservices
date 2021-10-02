// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import type { NextApiRequest, NextApiResponse } from 'next'

type Service= {
    id:number;
  "type": string;
  "badge_color":string;
  "publisher": string;
  "title": string;
  "description": string;
  "trust": number;
  pledge:number;
  "price"?: number;
  "deadline"?: number;
  "status"?: string;
  image?:string;
  questions?:string[];
}

type Data = {
  services: Service[]
}

const services_data = {
    "services": [
        {
            id:1,
            "type": "one-time",
            "badge_color":"red",
            "publisher": "35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3",
            "title": "I will write a 200 pages book",
            "description": "I have 45 years of experience as I writer, having written 5 best-sellers...",
            "trust": 30000,
            pledge:100,
            "price": "200.000",
            deadline: '14/08/2021',
            "status": "Running",
            image:"https://www.freecodecamp.org/news/content/images/2020/09/writing-cover.jpg",
            questions:[
                'Please provide content main target in terms of customers description and expectations',
                'Please inform a deadline from deal date',
                'Please inform number of languages you plan to translate the book into'
            ]
           
        },
        {
            id:2,
            "type": "constant",
            "badge_color":"green",
            "publisher": "977efb35ab621d39dbeb7274ec7795a34708ff4d25a01a1df04c1f27",
            "title": "Company Policies",
            "description": "A smart contract employer-employees to regulate bonus compensation",
            "trust": 10000,
            "deadline": '29/08/2021',
            "price": '170.000',
            "status": "Ended",
            pledge:100,
            image:"https://www.pcg-services.com/wp-content/uploads/2016/08/policies-procedures-1.jpg",
            questions:[
                'Are you committing to achieve yearly turnover > 10 mio USD?',
                'Are you available to assure availability over the week-ends?',
                'Are you ready to travel weekly over regions at distances > 200 km?'
            ]
        },
        {
            id:3,
            "type": "one-time",
            "badge_color":"red",
            "publisher": "7f8a76c0ebaa4ad20dfdcd51a5de070ab771f4bf377f2c41e6b71c0a",
            "title": "Mountain Bike For Sale",
            "description": "It was used for only 2 months, in very good state",
            "trust": 45000,
            "price": '80.000',
            pledge:100,
            "deadline": '14/08/2021',
            "status": "Running",
            image:"https://image.made-in-china.com/202f0j00zclUMRvBfCkm/MTB-Hot-Sale-21-Speed-Mountain-Bikes-Bicycle-High-Quality-Best-Price-MTB-Mountainbike-29-Inch-Adults-MTB-Bikes.jpg",
            questions:[
                'Please confirm availability to pick-up the bike at the indicated address',

            ]
        },
        {
            id:4,
            "type": "one-time",
            "badge_color":"red",
            "publisher": "bcc083ade3fdd0a372cb6c43ef00ef02fcb52e9532941117d7609d6a",
            "title": "I will do your Web Site Front-End",
            "description": "50 years of experience software engineering.\n I will make you a beautiful front page website for an affordable price.",
            "trust": 73200,
            "price": "1.000.000",
            pledge:150,
            "deadline": '15/08/2021',
            image:"https://cdn.billomat.com/wp-content/uploads/2019/09/freelance-web-developer-danial1-ricaros-FCHlYvR5gJI-unsplash.jpg",
            "status": "Ended"
            
        },
        {
            id:5,
            "type": "constant",
            "badge_color":"green",
            "publisher": "3f96cfba6b1a10bf4dec08ea1459b87ecefa65c65a5f0899d030a214",
            "title": "If elected, I will be an excellent congressman",
            "description": "I defined a set of targets I must complete if elected",
            "trust": "1000000",
            "price": '20.000',
            pledge:3000,
            "deadline": '17/09/2021',
            image:"https://direitosbrasil.com/wp-content/uploads/2016/09/marketing-politico.jpg",
            "status": "Ended",
        },
        {
            id:6,
            "type": "constant",
            "badge_color":"green",
            "publisher": "3f96cfba6b1a10bf4dec08ea1459b87ecefa65c65a5f0899d030a214",
            "title": "Holiday House",
            "description": "It's a beautiful 180 sqm apartment on the ground floor in an exclusive residential village located on the beautiful white beach in the heart of the Marine Park and Watamu Nature Reserve, right in front of the Island of Love, destination of all tourists.",
            "trust": "1000000",
            "price": '250.000',
            pledge:500,
            "deadline": '22/09/2021',
            image:"https://direitosbrasil.com/wp-content/uploads/2016/09/marketing-politico.jpg",
            "status": "Running",
            questions:[
                'Are you committing to leave the house in the same way you find as you enter it, proving it with pictures of the indicated locations in the attach?',
                'Are you available to assure that not more than 4 people will be leaving in the house ?'
            ]
        }

    ]
  }

export function services(){
  return services_data 
}


export function service(id:number){
  return services_data.services.filter(service => service.id === id) 
}



