      subroutine LEAFY
     $(NO,TRIN,IQSFS,JH1,JH2,IZERO)
C
C     Rudolf Loeser, 2005 Oct 14
C---- Prints explanations, for YODEL.
C     !DASH
      save
C     !DASH
      integer IQSFS, IZERO, JH1, JH2, NO
      logical TRIN
C     !DASH
      external LINER, VARAN, HI, BYE
C
      call HI ('LEAFY')
C     !BEG
      if(NO.gt.0) then
        if(TRIN) then
          call LINER (2, NO)
          write (NO,100)
  100     format(' ','N O T E S')
          call LINER (1, NO)
          call VARAN (NO, IZERO, 'JZATMO')
C     !EJECT
          write (NO,101)
  101     format(' ','"Photoionization Rates Multiplier":'/
     $           ' ',10X,'Since Radiation Temperatures are used, ',
     $               'Photoionization Rates (and corresponding ',
     $               'bound-free Heating Rates)'/
     $           ' ',10X,'use the Mean Intensity  J = MULT * B(TR),  ',
     $               'where B is the Planck Function, ',
     $               'and  MULT = H * D  is the'/
     $           ' ',10X,'Photoionization Rates Multiplier printed ',
     $               'above.')
          if(JH2.le.0) then
            write (NO,102)
  102       format(' ',18X,'Since JH2 .le. 0 in this run, ',
     $                 'MULT = 1 everywhere.')
          else
            write (NO,103) JH1,JH2
  103       format(' ',10X,'Its factors  H  and  D  are ',
     $                 'determined as follows:'/
     $             ' ',10X,'"H" is computed from the input parameters ',
     $                 'JH1 =',I4,' and JH2 =',I4,', such that:'/
     $             ' ',10X,'for depth indices less than JH1, H=0.5; ',
     $                 'for depth indices greater than ',
     $                 'JH2, H=1; and'/
     $             ' ',10X,'linear interpolation vs. depth index ',
     $                 'is used in the range from JH1 to JH2.')
            if(JH2.eq.1) then
              write (NO,104)
  104         format(' ',18X,'Since JH2 .eq. 1 in this run, ',
     $                       'H=1 everywhere.')
            end if
            write (NO,105)
  105       format(' ',10X,'"D" depends on the geometry of the ',
     $                 'atmospheric model, as follows:')
            if(IQSFS.gt.0) then
              write (NO,106)
  106         format(' ',10X,'Spherical model, therefore ',
     $                   'D = 1 - (1 - R**-2)**1/2  , where R is ',
     $                   '"Distance (Radii)" printed above.')
            else
              write (NO,107)
  107         format(' ',10X,'Plane-parallel model, therefore D = 1.')
            end if
          end if
        end if
      end if
C     !END
      call BYE ('LEAFY')
C
      return
      end
