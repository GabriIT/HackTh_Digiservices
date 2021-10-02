import React, { ReactChildren } from 'react'

import style from './style'

interface EventProps{
    title?:string;
    subtitle?:string;
    interval?:string;
    children?:string;
}

type Props = {
    children: JSX.Element | JSX.Element[],
};

export const Timeline = ( {children}:Props) =>
  <div className={style.container}>
    <ul className={style.timeline}>
      {children}
    </ul>
  </div>


export const Event = ({ title, subtitle, interval, children }:EventProps) =>
  <li className={style.event}>
    <label className={style.icon}></label>
    <div className={style.body}>
      <p className={style.date}>{interval}</p>
      <h3>{title}</h3>
      {subtitle && <h4>{subtitle}</h4>}
      <div className={style.description}>
        {children}
      </div>
    </div>
  </li>