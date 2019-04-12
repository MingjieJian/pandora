      subroutine HIMRA
     $(IQHSE,KTKIN,N,LHHSE)
C
C     Rudolf Loeser, 2006 Dec 20
C---- Checks value of LHHSE.
C     !DASH
      save
C     !DASH
      integer IQHSE, KTKIN, LHHSE, N
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      call HI ('HIMRA')
C     !BEG
      if((IQHSE.gt.0).and.(KTKIN.eq.0)) then
        if(LHHSE.ne.1) then
          if((LHHSE.lt.1).or.(LHHSE.gt.N)) then
            write (MSSLIN(1),100) N, LHHSE
  100       format('N =',I8,'; LHHSE =',I10,' is invalid.')
            call HALT ('HIMRA', 1)
          end if
        end if
      end if
C     !END
      call BYE ('HIMRA')
C
      return
      end
