      subroutine NOTICE
     $(NW,A,B,CA,CB,CD)
C     Rudolf Loeser, 1989 Dec 18
C---- Displays A and B according to "1PE(NW).(ND)" (where ND=NW-8),
C     and also their "differences".
C     CA, CB and CD must each be of type character*NW.
C     !DASH
      save
C     !DASH
      real*8 A, B
      integer J, K, ND, NW
      character BIGX*1, BLANK*1, CA*(*), CB*(*), CD*(*), FORMAT*10,
     $          SMALLX*1, UNDER*1, WEX1*3, WEX2*2
C     !DASH
      external ABORT
C
      data BLANK,BIGX,SMALLX,UNDER /' ', 'X', 'x', '_'/
      data WEX1,WEX2 /'x__', '__'/
C
C     !BEG
      if((NW.lt.8).or.(NW.gt.99)) then
        write (*,100) NW
  100   format(' ','NOTICE:  NW =',I12,', which is not between 8 ',
     $             'and 99, inclusive.')
        call ABORT
      end if
      ND = NW-8
C
      write (FORMAT,101) NW,ND
  101 format('(1PE',I2,'.',I2,')')
      write (CA,FORMAT) A
      write (CB,FORMAT) B
C
      if(CA.eq.CB) then
        CD = BLANK
        K  = (NW-4)/2
        if(A.eq.B) then
          CD(K:K+4) = 'exact'
        else
          CD(K:K+3) = 'same'
        end if
        goto 103
      end if
C     !EJECT
      CD(1:4) = CA(1:4)
      if(CA(2:2).ne.CB(2:2)) then
        CD(2:2) = SMALLX
      end if
      if(CA(3:3).ne.CB(3:3)) then
        CD(3:3) = BIGX
      end if
C
      if(ND.gt.0) then
        J = 0
        if(CD(3:3).eq.BIGX) then
          J = 1
        end if
        do 102 K = 5,(NW-4)
          if(J.eq.0) then
            if(CA(K:K).eq.CB(K:K)) then
              CD(K:K) = CA(K:K)
            else
              J=1
            end if
          end if
          if(J.gt.0) then
            CD(K:K) = UNDER
          end if
  102   continue
      end if
C
      K = NW-3
      CD(K:NW) = CA(K:NW)
      K = NW-2
      if(CA(K:NW).ne.CB(K:NW)) then
        if(CA(K:K).ne.CB(K:K)) then
          CD(K:NW) = WEX1
        else
          K = NW-1
          CD(K:NW) = WEX2
        end if
      end if
C
  103 continue
C     !END
C
      return
      end
