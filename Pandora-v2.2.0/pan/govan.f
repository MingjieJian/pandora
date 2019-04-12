      subroutine GOVAN
     $(NO,N,NL,Z,TE,GD,CHL,CHT,MODE,SUM)
C
C     Rudolf Loeser, 1982 Feb 24
C---- Prints heating rates.
C     !DASH
      save
C     !DASH
      real*8 CHL, CHT, GD, SUM, TE, Z
      integer I, IE, IL, IS, IU, IUL, J, MODE, N, NL, NO
      character QD*15, QGD*15, QSU*15, QT*15, QZ*15
C     !DASH
      external  MADAI, LINER, INDXUL, MIMSY, HI, BYE
      intrinsic min
C
C               Z(N), TE(N), CHL(N,NL), CHT(N,MUL), GD(N), SUM(N)
      dimension Z(*), TE(*), CHL(N,*),  CHT(N,*),   GD(*), SUM(*)
C
      data QD,      QT,            QGD,           QSU /
     $     'Depth', 'Temperature', 'Gas Density', 'Total Rate'/
C     !EJECT
C
      call HI ('GOVAN')
C     !BEG
      if(NO.gt.0) then
        call MADAI        (NO,MODE,'COLLISIONAL','HEATING')
C
        IE = 0
  100   continue
          IS  = IE+1
          IE  = min(IE+8,N)
C
          call LINER      (2,NO)
          write (NO,101) (I,I=IS,IE)
  101     format(' ',15X,8I13)
  102     format(' ',A15,1P8E13.5)
          call LINER      (1,NO)
          write (NO,102) QD ,(Z(I) ,I=IS,IE)
          write (NO,102) QT ,(TE(I),I=IS,IE)
          write (NO,102) QGD,(GD(I),I=IS,IE)
          call MIMSY      (NO,MODE)
C
          do 104 J = 1,NL
            write (QZ,103) J
  103       format(I2,'/ K',10X)
            write  (NO,102) QZ,(CHL(I,J),I=IS,IE)
  104     continue
C
          call LINER      (1,NO)
          do 107 IU = 2,NL
            do 106 IL = 1,(IU-1)
              call INDXUL (IU,IL,IUL)
              write (QZ,105) IU,IL
  105         format(I2,'/',I2,10X)
              write (NO,102) QZ,(CHT(I,IUL),I=IS,IE)
  106       continue
  107     continue
C
          call LINER      (1,NO)
          write (NO,102) QSU,(SUM(I),I=IS,IE)
        if(IE.lt.N) goto 100
C
      end if
C     !END
      call BYE ('GOVAN')
C
      return
      end
