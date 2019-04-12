      subroutine VOLE
     $(A,N,LAB,K,XLM,IADRS,DUMP)
C
C     Rudolf Loeser, 2003 Feb 14
C---- Checks opacities, for DEVIL.
C     !DASH
      save
C     !DASH
      real*8 A, XLM
      integer IADRS, K, LUEO, N
      logical AZERO, DUMP
      character LAB*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external NAUGHTD, MESHED, VECOUT, MASHED, HI, BYE
C
C               A(N)
      dimension A(*)
C
      call HI ('VOLE')
C     !BEG
      call NAUGHTD  (A, 1, N, AZERO)
C
      if(AZERO) then
        if(.not.DUMP) then
          call MESHED ('VOLE', 1)
        end if
C
        write (LUEO,100) K,XLM,IADRS
  100   format(' ','Error in "LYMAN" at',I5,'. frequency: opacity ',
     $             'is zero.',5X,'XLM =',1PE24.16,5X,'iadrs =',I10)
        call VECOUT (LUEO, A, N, LAB)
C
        if(.not.DUMP) then
          call MASHED ('VOLE')
        end if
      end if
C     !END
      call BYE ('VOLE')
C
      return
      end
