class Main{
    main():Int{ 
            let x:Int<- 0 , y:Int <- 0-1, a:Int, z:Int in {
                z <- ~(1+(2*3)/4-5);
                z <- 1+if a=2 then 3 else 4 fi;
                a <- 5+if not a=6 then 7 else 8 fi;
                a <- 1;
                if 0<=a then {
                    while a<=10 loop {
                        x<- x+a;
                    } pool ;
                    x;
                }
                else{
                    while a<10 loop {
                        x<- x+a;
                    } pool ;
                    x;
                }
                fi;
                x;
                x+2;
                x+2;
            }
    };
};