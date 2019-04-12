      subroutine TRILL
     $(NO,HEAD,INCDNT,JNUMTH,SHORT)
C
C     Rudolf Loeser, 1980 Mar 13
C---- Prints a heading, for SHOOT.
C     !DASH
      save
C     !DASH
      integer I, IL, IT, JNUMTH, NO, jummy
      logical INCDNT, SHORT
      character HEAD*12, LEG*21, OPT*3, TEXT*9
C     !DASH
      external  LINER, ONOFF, HI, BYE
      intrinsic max, min
C
      dimension TEXT(3,2), LEG(2)
C
      data TEXT /'         ', '         ', '         ',
     $           ' Incident', 'Radiation', '   Term  '/
C
      data LEG  /'from source function.',
     $           'directly.            '/
C     !EJECT
C
      call HI ('TRILL')
C     !BEG
      if(NO.gt.0) then
        call ONOFF   (JNUMTH, jummy, OPT)
        IL = max(min((JNUMTH+1),2),1)
C
        if(SHORT) then
          call LINER (1, NO)
          write (NO,100) OPT,LEG(IL)
  100     format(' ','Option USENCJ = ',A3,': Jnu computed ',A21)
        else
C
          call LINER (4, NO)
          write (NO,101) HEAD,OPT,LEG(IL)
  101     format(' ','Mean Intensity and Continuum Source Function at ',
     $               A12,'; option USENCJ = ',A3,': Jnu computed ',A21)
          call LINER (2, NO)
C
          IT = 1
          if(INCDNT) then
            IT = 2
          end if
C
          write (NO,102) (TEXT(I,IT),I=1,3)
  102     format(' ',64X,'Continuum',3X,'Absrption',33X,A9/
     $           ' ',5X,'Scattering',26X,'Optical',7X,'Mean',
     $               7X,'Source',6X,'Source',6X,'Planck',15X,'Flux',
     $               3X,A9/
     $           ' ',8X,'Ratio',6X,'Opacity',4X,'Height',6X,'Depth',
     $               5X,'Intensity',4X,'Function',4X,'Function',
     $               4X,'Function',4X,'Temp.',4X,'Deriv.',2X,A9)
          call LINER (1, NO)
        end if
      end if
C     !END
      call BYE ('TRILL')
C
      return
      end
