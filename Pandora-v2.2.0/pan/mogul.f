      subroutine MOGUL
     $(WVLA,WVLM,HEAD)
C
C     Rudolf Loeser, 1980 Nov 06
C---- Encodes a wavelength value, for FRONT.
C     !DASH
      save
C     !DASH
      real*8 A, B, F, P, WVLA, WVLM, ZERO
      integer KOUNT, LUEO
      character FMT*16, HEAD*12, PRE*5, PRO*5
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external SIPRFX, RIGHT, MESHED, MASHED, HI, BYE
C
      dimension A(3), B(3)
C
      data A / 9.9995D0,  99.995D0,  1000.D0/
      data B /-9.995D0,  -99.95D0,  -1000.D0 /
      data FMT /'(F5.d,1X,A5,''M'')'/
C     !EJECT
C
      call HI ('MOGUL')
C     !BEG
      call SIPRFX   (WVLM,F,P,1,PRO)
      call RIGHT    (PRO,PRE,5)
C
      KOUNT = -1
      if(F.eq.ZERO) then
        KOUNT = 1
      else if(F.gt.ZERO) then
        if(F.lt.A(1)) then
          KOUNT = 3
        else if(F.lt.A(2)) then
          KOUNT = 2
        else if(F.lt.A(3)) then
          KOUNT = 1
        end if
      else
        if(F.gt.B(1)) then
          KOUNT = 2
        else if(F.gt.B(2)) then
          KOUNT = 1
        else if(F.gt.B(3)) then
          KOUNT = 0
        end if
      end if
C
      if(KOUNT.ge.0) then
        write (FMT(5:5),100) KOUNT
  100   format(I1)
        write (HEAD,FMT) F,PRE
      else
        HEAD = '  nonsense   '
C
        call MESHED ('MOGUL', 3)
        write(LUEO,101)WVLA,WVLM,F,P,PRE
  101   format(' ','Trouble: ',1P4E20.12,5X,A5)
        call MASHED ('MOGUL')
      end if
C     !END
      call BYE ('MOGUL')
C
      return
      end
