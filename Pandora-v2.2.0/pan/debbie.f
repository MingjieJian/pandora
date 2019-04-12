      subroutine DEBBIE
     $(GRAF,HEAD,N,NOPAC,Z,C,NO,ZL,CL,NAME,TAUK,LYM,R1,RR,PLTID)
C
C     Rudolf Loeser, 1973 Feb 06
C---- Plots relative contributions vs. depth.
C     !DASH
      save
C     !DASH
      real*8 C, CL, R1, RR, TAUK, Z, ZL
      integer KOUNT, N, NO, NOPAC
      logical GRAF, LYM
      character HEAD*12, LABEL*127, NAME*9, PLTID*1, TIT*10
C     !COM
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C     !DASH
      external GRIN, GABOR, KPRINT, ABJECT, LINER, HI, BYE
C
C               ZL(N), R1(N), RR(N), CL(N,Nopac), C(Nopac,N), PLTID(4),
      dimension ZL(*), R1(*), RR(*), CL(*),       C(*),       PLTID(*),
C
C               Z(N), TAUK(N)
     $          Z(*), TAUK(*)
C
      call HI ('DEBBIE')
C     !BEG
      if(GRAF.and.(NO.gt.0)) then
        call GRIN     (N, Z, ZL, C, CL, IMAGE, TIT, TAUK, LYM, R1, RR,
     $                 KOUNT, PLTID)
C
        write (LABEL,100) NAME,TIT,HEAD
  100   format(' ','Plot of log(Relative Contributions of ',A9,') vs. ',
     $             A10,', at wavelength = ',A12)
C
        if(KOUNT.gt.182) then
          call ABJECT (NO)
          write (NO,101) LABEL
  101     format(' ',A)
          call LINER  (1, NO)
          call KPRINT (IMAGE, NO)
        else
          call GABOR  (NO, 3, 0, LABEL)
        end if
      end if
C     !END
      call BYE ('DEBBIE')
C
      return
      end
