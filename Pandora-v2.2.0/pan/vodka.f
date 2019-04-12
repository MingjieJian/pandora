      subroutine VODKA
     $(EPN,CRIT,N,METSE,METNEW,MM,KNT)
C
C     Rudolf Loeser, 1984 Oct 24
C---- Picks a new Statistical Equilibrium method.
C     (This is version 3 of VODKA.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, EPN
      integer J, KNT, MET, METNEW, METSE, MM, N
      logical BAD
C     !DASH
      external VEERY, HI, BYE
C
C               EPN(N,KNT), MM(KNT)
      dimension EPN(N,*),   MM(*)
C
      call HI ('VODKA')
C     !BEG
      do 100 J = 1,KNT
        MET = MM(J)
        if(MET.ne.2) then
C
          call VEERY (EPN(1,(MET+1)),CRIT,N,BAD)
          if(.not.BAD) then
            METNEW = MET
            goto 101
          end if
C
        end if
  100 continue
      METNEW = METSE
C
  101 continue
C     !END
      call BYE ('VODKA')
C
      return
      end
