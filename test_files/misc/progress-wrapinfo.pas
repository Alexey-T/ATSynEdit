        //no help, still wrapinfo calcing is small time,
        //and big time, after loading Strings, is unknown reason.
        //this is on "linelen 70K.txt".
        if FPaintLocked>0 then
        begin
          NProgress:= Int64(i)*100 div (Strings.Count+1);
          if Abs(NProgress-FWrapProgress)>=cMinIncForWrapProgress then
          begin
            FWrapProgress:= NProgress;
            DoShowProgress(nil);
          end;
        end;
