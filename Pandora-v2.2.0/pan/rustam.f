      subroutine RUSTAM
     $(XI,K,KM,XIA,KA,CRIT)
C
C     Rudolf Loeser, 2003 Mar 11
C---- Adds appropriate candidate values to the final XI-table,
C     for PALLE.
C     !DASH
      save
C     !DASH
      real*8 CRIT, XI, XI1, XIA, XIK
      integer I, K, KA, KM, LOOK, jummy
      logical YES
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external WITHIN, HALT, LOOKUD, HI, BYE
C
C               XI(KM), XIA(KM)
      dimension XI(*),  XIA(*)
C
      call HI ('RUSTAM')
C     !BEG
      XI1 = XI(1)
      XIK = XI(K)
      do 101 I = 1,KA
        call WITHIN     (XI1, XIA(I), XIK, 1, YES)
        if(YES) then
          call LOOKUD   (XI, K, CRIT, XIA(I), jummy, LOOK)
C
          if(LOOK.eq.2) then
            if(K.lt.KM) then
              K = K+1
              XI(K) = XIA(I)
C
            else
              write (MSSLIN(1),100) I,KA,KM
  100         format('I =',I12,', KA =',I12,'; KM =',I12,
     $               ', which is too small.')
              call HALT ('RUSTAM', 1)
            end if
          end if
C
        end if
  101 continue
C     !END
      call BYE ('RUSTAM')
C
      return
      end
