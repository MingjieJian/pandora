      subroutine RR
     $(DT,J,TERM,SE2,SE3,SES2,SES3,KODE)
C
C     Rudolf Loeser, 1971 Jul 07
C---- Computes the term RR, for the RT weight matrix.
C     !DASH
      save
C     !DASH
      real*8 DT, E2J, E3J, E3JM, HALF, HNDRD, HNDRDTH, RAT, SE2, SE3,
     $       SES2, SES3, TERM, TM, ZERO
      integer J, JM, KODE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external HICKORY, WALNUT, DIVIDE, HI, BYE
C
C               DT(2*N), SE2(2*N), SE3(2*N), SES2(2*N), SES3(2*N)
      dimension DT(*),   SE2(*),   SE3(*),   SES2(*),   SES3(*)
C
      data HNDRDTH,HNDRD /1.D-2, 1.D2/
C     !EJECT
C
      call HI ('RR')
C     !BEG
      JM = J-1
      TM = DT(J)-DT(JM)
C
      if(DT(JM).ge.HNDRD) then
        KODE = 0
        TERM = ZERO
C
      else
C
        KODE = 1
        if(DT(J).lt.HNDRDTH) then
          call HICKORY (2,J ,DT,SES2,E2J )
          call HICKORY (3,J ,DT,SES3,E3J )
          call HICKORY (3,JM,DT,SES3,E3JM)
        else
          call WALNUT  (2,J ,DT,SE2,E2J )
          call WALNUT  (3,J ,DT,SE3,E3J )
          call WALNUT  (3,JM,DT,SE3,E3JM)
        end if
        call DIVIDE    ((E3JM-E3J),TM,RAT)
        TERM = HALF*(RAT-E2J)
      end if
C     !END
      call BYE ('RR')
C
      return
      end
