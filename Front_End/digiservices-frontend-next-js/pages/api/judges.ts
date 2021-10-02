// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import type { NextApiRequest, NextApiResponse } from 'next'

type Judge= {
  name: string;
  description: string;
  trust: number;
  price: number;
  image?:string;
  isJudge: boolean;
}

type Data = {
  judges: Judge[]
}

export function judges(){
  return {
    "judges": [
        {
            "name": "Halina Evelyn",
            "description": "Dark stubble seductively compliments his eyes and leaves a beautiful memory of his luck in battles.",
            "trust": 30000,
            "price": 20,
            image:"https://www.freecodecamp.org/news/content/images/2020/09/writing-cover.jpg",
            isJudge: true
        },
        {
            "name": "Gaea Micheil",
            "description": "This is the face of Alluin Moonwalker, a true adventurer among blood elves. He stands tiny among others, despite his bulky frame.",
            "trust": 15000,
            "price": 15,
            image:"https://static.generated.photos/vue-static/face-generator/landing/wall/14.jpg",
            isJudge: false
        },
        {
            "name": "Iesha Ema",
            "description": "Soft skin graciously compliments her hair and cheekbones and and leaves an intriguing memory of her luck.",
            "trust": 44700,
            "price": 35,
            image:"https://pbs.twimg.com/profile_images/1186629883910328320/XMquc2OW.jpg",
            isJudge: false
        },
        {
            "name": "Frans Adam",
            "description": "Freckles are spread handsomely around his cheeks and leaves a pleasant memory of his adventurous love life.",
            "trust": 45900,
            "price": 80,
            image:"https://static.generated.photos/vue-static/face-generator/landing/demo-previews/sex.jpg",
            isJudge: true
        },
        {
           "name": "Yngvildr Amyntas",
            "description": "IAn old tattoo resembling a skull is prominently featured on the side of his right cheekbone leaves a painful burden of deceased loved ones.",
            "trust": 27800,
            "price": 100,
            image:"https://www.criptofacil.com/wp-content/uploads/2021/05/satoshi-nakamoto-e-homenageado-com-curiosa-estatua.jpg",
            isJudge: false
        }
    ]
  }
}

