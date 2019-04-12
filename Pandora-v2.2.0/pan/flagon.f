      subroutine FLAGON
     $(LU)
C
C     Rudolf Loeser, 1984 Sep 24
C---- Prints message from "DIVIDE".
C     (This is version 2 of FLAGON.)
C     !DASH
      save
C     !DASH
      integer LU
C     !COM
C---- DIVIDER     as of 2003 Sep 29
      real*8      VSMLDV
      integer     MESDIV,MESDVZ,KNTDIV,KNTDVZ
      common      /DIVIDE1/ VSMLDV
      common      /DIVIDE2/ MESDIV,MESDVZ,KNTDIV,KNTDVZ
C     Parameters for subroutine "DIVIDE":
C       VSMLDV - replacement for B in A/B when B=0;
C       MESDIV - switch for A/0 error messages;
C       MESDVZ - switch for 0/0 error messages;
C       KNTDIV - number of times A/0 was detected;
C       KNTDVZ - number of times 0/0 was detected.
C     .
C     !DASH
      external LINER, HI, BYE
C
      call HI ('FLAGON')
C     !BEG
      if((LU.gt.0).and.((KNTDIV.gt.0).or.(KNTDVZ.gt.0))) then
        call LINER (2,LU)
        if(KNTDIV.le.0) then
          write (LU,100)
  100     format(' ','When computing A/B in "DIVIDE", A/0 was found',
     $               ' 0 times;')
        else
          write (LU,101) KNTDIV,VSMLDV
  101     format(' ','When computing A/B in "DIVIDE", A/0 was found',
     $               I10,' times, (B was replaced by',1PE10.2,');')
        end if
        write (LU,102) KNTDVZ
  102   format(' ',32X,'0/0 was found',I10,' times.')
        if((KNTDIV.gt.0).or.(KNTDVZ.gt.0)) then
          write (LU,103)
  103     format(' ','To print a message every time A/0 is found, ',
     $               'use IPZER = 1;'/
     $           ' ','to print a message every time 0/0 is found, ',
     $               'use IPZER = 2;'/
     $           ' ','for both messages, use IPZER = 3;'/
     $           ' ','for no messages, use IPZER = 0 ',
     $               '(which is the default setting).')
        end if
      end if
C     !END
      call BYE ('FLAGON')
C
      return
      end
