export default function debounce(limit:number, callback:()=>void) {
    let timeoutId: number;
    return (...args:any) => {
      if (timeoutId) {
        clearTimeout(timeoutId)
      }
      timeoutId = setTimeout(callback, limit, args)
    }
  }