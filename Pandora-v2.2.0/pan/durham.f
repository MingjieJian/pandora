      subroutine DURHAM
     $(NMT,N,PFT,ELSYM,NSUB,PFE,ELED,RAT,IPNT)
C
C     Rudolf Loeser, 1984 Aug 16
C---- Makes edited PFT table, for plotting.
C     (This is version 2 of DURHAM.)
C     !DASH
      save
C     !DASH
      real*8 PFE, PFT, RAT
      integer I, IPNT, J, N, NMT, NSUB
      character ELED*3, ELSYM*3
C     !DASH
      external ELAND, MOVEC, MOVE1, HI, BYE
C
C               PFT(N,NMT), PFE(N,NMT), IPNT(NMT), RAT(NMT), ELED(NMT),
      dimension PFT(N,*),   PFE(N,*),   IPNT(*),   RAT(*),   ELED(*),
C
C               ELSYM(NMT)
     $          ELSYM(*)
C
C
      call HI ('DURHAM')
C     !BEG
      if(NMT.eq.NSUB) then
C----   Use table as is
        call MOVE1   (PFT,(N*NMT),PFE)
        call MOVEC   (ELSYM,1,NMT,ELED,1,NSUB)
      else
C----   Get indices of ions to be used
        call ELAND   (NMT,N,PFT,IPNT,RAT,NSUB)
C----   Set up edited table
        do 100 J = 1,NSUB
          I = IPNT(J)
          ELED(J) = ELSYM(I)
          call MOVE1 (PFT(1,I),N,PFE(1,J))
  100   continue
      end if
C     !END
      call BYE ('DURHAM')
C
      return
      end
