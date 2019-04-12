      subroutine REKKAN
     $(N,IU,IL,MBD,BDJ,BDR,BDQ,BDS,BTIT,ABDR)
C
C     Rudolf Loeser, 2003 Apr 09
C---- Sets up RBD values for printing, for KRAKEN.
C     (See also KANKER.)
C     !DASH
      save
C     !DASH
      real*8 ABDR, BDJ, BDQ, BDR, BDS
      integer I, IL, INDEX, IU, J, K, MBD, N
      character BTIT*4, TIT*4
C     !DASH
      external BRAT, HI, BYE
C
C               BDJ(N,NL), BDR(N,NL), BDQ(N,NL), BDS(N), ABDR(N,3),
      dimension BDJ(*),    BDR(*),    BDQ(*),    BDS(*), ABDR(N,*),
C
C               BTIT(4)
     $          BTIT(*)
C
      dimension TIT(4)
C
      data TIT /'RBDJ', 'RBDR', 'RBDQ', 'RBDS'/
C
      call HI ('REKKAN')
C     !BEG
      INDEX   = MBD+1
      BTIT(4) = TIT(INDEX)
C
      J = 0
      do 101 K = 1,4
        if(K.ne.INDEX) then
          J = J+1
          BTIT(J) = TIT(K)
          do 100 I = 1,N
            if(K.eq.1) then
              call BRAT (I, IU, IL, BDJ, ABDR(I,J))
C
            else if(K.eq.2) then
              call BRAT (I, IU, IL, BDR, ABDR(I,J))
C
            else if(K.eq.3) then
              call BRAT (I, IU, IL, BDQ, ABDR(I,J))
C
            else
              ABDR(I,J) = BDS(I)
            end if
  100     continue
        end if
  101 continue
C     !END
      call BYE ('REKKAN')
C
      return
      end
