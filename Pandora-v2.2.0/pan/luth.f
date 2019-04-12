      subroutine LUTH
     $(ISUB,IOMX,LYMIT,IHSLT,ION,WPOP,WBD,NBS,IWEIT,JEDIT,MBREC,JNEDP,
     $ IBNVW,NO)
C
C     Rudolf Loeser, 1986 Oct 31
C---- Prints iteration counts and weights.
C     !DASH
      save
C     !DASH
      real*8 WBD, WPOP
      integer IBNVW, IHSLT, IOMX, ISUB, IWEIT, JEDIT, JNEDP, LYMIT,
     $        MBREC, NBS, NO
      logical ION
C     !DASH
      external LINER, HI, BYE
C
      call HI ('LUTH')
C     !BEG
      if(NO.gt.0) then
        call LINER   (2,NO)
        write (NO,100) IOMX,ISUB,IHSLT,LYMIT
  100   format(' ',I3,' overall iterations;'/
     $         ' ',I3,' subiterations (i.e. for RHO and BD);'/
     $         ' ',I3,' HSL iterations (i.e. HSE - Lyman loop);'/
     $         ' ',I3,' Lyman iterations.')
C
        if(ION) then
          call LINER (1,NO)
          write (NO,101) WPOP,WBD,NBS,IWEIT
  101     format(' ','Weights for successive iterates:',
     $               10X,'(weight = 1 means: ',
     $               'do not use the previous iterate)'//
     $           ' ',13X,'WPOP =',1PE12.4,' weight for NK and ND;'/
     $           ' ',13X,'WBD  =',  E12.4,' weight for BD;   NBS =',I3,
     $               ' level index for BD-smoothing.'//
     $           ' ','Weighting details (for final RHO, and Lyman B ',
     $               'and RK) are printed depending on IWEIT'/
     $           ' ','(IWEIT = 0: none;  IWEIT = 1: last iteration ',
     $               'only; IWEIT = 2: always). In this run'/
     $           ' ',19X,'IWEIT =',I2)
          call LINER (1,NO)
          write (NO,102) JEDIT,MBREC,JNEDP,IBNVW
  102     format(' ','JEDIT   =',I8,', the depth index where ',
     $               'N-editing starts (option NEDIT)'/
     $           ' ','MBREC   =',I8,', switch for B-editing (to ',
     $               'maintain consistency with N-edited ND)'/
     $           ' ','JNEDP   =',I8,', switch for detailed output ',
     $               'from N- and B-editing'//
     $           ' ','IBNVIEW =',I8,', the depth index for printed ',
     $               'details of BD & ND calculations')
        end if
      end if
C     !END
      call BYE ('LUTH')
C
      return
      end
