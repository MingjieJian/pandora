      subroutine POTOO
     $(NO,N,NL,XJ,DIV,NV,VMIN,Z,TE,S,EV,E,FCK,FCJ)
C
C     Rudolf Loeser, 1984 Jul 21
C---- Prints, for GREYLAG.
C     !DASH
      save
C     !DASH
      real*8 DIV, E, EV, FCJ, FCK, S, TE, VMIN, XJ, Z
      integer I, IE, IS, J, N, NL, NO, NV
      character TAT*7, TIT*7
C     !DASH
      external  ABJECT, LINER, HI, BYE
      intrinsic min
C
C               Z(N), TE(N), S(N), EV(N), E(N), FCK(N), FCJ(N,NL)
      dimension Z(*), TE(*), S(*), EV(*), E(*), FCK(*), FCJ(N,*)
C
      dimension TIT(6)
C
      data TIT /'Z      ', 'TE     ', 'S      ', 'EV     ',
     $          'E      ', 'FCK1   '/
C
      call HI ('POTOO')
C     !BEG
      if(NO.gt.0) then
        call ABJECT (NO)
        write (NO,100) XJ,VMIN,NV,DIV
  100   format(' ','Hydrogen collisional excitation and ionization ',
     $             'rates due to fast electrons.'//
     $         ' ','Electron flux =',1PE16.8//
     $         ' ','(Velocity sampling parameters: VMNFE =',E16.8,5X,
     $             'NVF =',I3,'.)'//
     $         ' ','(E = EV / DIV, where DIV =',E16.8,'.)')
C     !EJECT
        IE  = 0
  101   continue
          IS = IE+1
          IE = min((IE+10),N)
C
          call LINER (2,NO)
          write (NO,102) (I,I=IS,IE)
  102     format(' ','  Depth',10I12)
          call LINER (1,NO)
C
  103     format(' ',A7,1P10E12.4)
          write (NO,103) TIT(1),(Z  (I),I=IS,IE)
          write (NO,103) TIT(2),(TE (I),I=IS,IE)
          write (NO,103) TIT(3),(S  (I),I=IS,IE)
          write (NO,103) TIT(4),(EV (I),I=IS,IE)
          write (NO,103) TIT(5),(E  (I),I=IS,IE)
          call LINER (1,NO)
          write (NO,103) TIT(6),(FCK(I),I=IS,IE)
          call LINER (1,NO)
C
          do 106 J = 2,NL
C
            if(J.lt.10) then
              write (TAT,104) J
  104         format('FC1',I1,3X)
            else
              write (TAT,105) J
  105         format('FC1',I2,2X)
            end if
C
            write (NO,103) TAT,(FCJ(I,J),I=IS,IE)
  106     continue
        if(IE.lt.N) goto 101
C
      end if
C     !END
      call BYE ('POTOO')
C
      return
      end
