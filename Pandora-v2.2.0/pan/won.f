      subroutine WON
     $(NL,WEIGHT,NO)
C
C     Rudolf Loeser, 1981 Jan 02
C---- Prints non-zero values of W.
C     (This is version 2 of WON.)
C     !DASH
      save
C     !DASH
      real*8 WEIGHT, WVAL, ZERO
      integer IL, IU, JL, JU, NL, NO
      logical JILROY, KILROY
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external WAITER, PADMA, LINER, HI, BYE
C
C               WEIGHT(MUL,NT)
      dimension WEIGHT(*)
C
      call HI ('WON')
C     !BEG
      if(NO.gt.0) then
C
        KILROY = .true.
        do 106 IU = 2,NL
          do 105 IL = 1,(IU-1)
            JILROY=.true.
            do 104 JU = 2,NL
              do 103 JL = 1,(JU-1)
                call WAITER      (JU, JL, IU, IL, 2, WEIGHT, WVAL)
                if(WVAL.ne.ZERO) then
                  if(KILROY) then
                    KILROY = .false.
                      call PADMA (NO,
     $                            'Statistical Equilibrium calculation')
                      write (NO,100)
  100                 format(' ','The calculation of "Z" for the ',
     $                           'statistical equilibrium equations, ',
     $                           'will use the following values of W:')
                  end if
C
                  if(JILROY) then
                    JILROY = .false.
                    call LINER   (1, NO)
                    write (NO,101) IU,IL,JU,JL,WVAL
  101               format(' ','Transition',I5,'/',I2,5X,
     $                         'Term',I5,'/',I2,5X,' =',F5.1)
                  else
                    write (NO,102) JU,JL,WVAL
  102               format(' ',27X,I5,'/',I2,5X,'W =',F5.1)
                  end if
C
                end if
  103         continue
  104       continue
  105     continue
  106   continue
C
      end if
C     !END
      call BYE ('WON')
C
      return
      end
