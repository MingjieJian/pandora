      subroutine POPUP
     $(X,W,ABDEL,XPBL)
C
C     Rudolf Loeser, 1971 Mar 02
C---- Updates population data.
C     !DASH
      save
C     !DASH
      real*8 ABD, ABDEL, W, X, XPBL
      integer I, JJBDI, JJBTL, JJH1, JJHND, JJNK, JJTE, JJXND, JJXNE,
     $        LLBD, LLPOPK, LLPOPN, N, NL, jummy
      character QELSM*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 50),JJNK )
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ(137),JJBTL)
      equivalence (IZOQ(124),JJH1 )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (QZQ(  2),QELSM)
      equivalence (RZQ(  6),ABD  )
C     !EJECT
C---- POPDATA     as of 2007 Jan 12
      integer     NPI
      parameter   (NPI=14)
C     (Remember to recompile all users when changing NPI.)
      real*8      POPMSS
      integer     LZOQ,MRTP,NPOPS,MAXPOPL,LENPBL,MRTPA,MRTPM,LIMPOP,
     $            LENT,NAMKNT,LENPOP,ICKSM,IUPOP,IBLAD,IPSWICH,KAPNO
      character   NAMES*10,TNAMES*8,POPSYM*3,KLABPI*8,NLABPI*8,BLABPI*8
      dimension   LZOQ(5), MRTP(50),
     $            LIMPOP(NPI), NAMKNT(NPI), LENPOP(NPI), IBLAD(NPI),
     $            ICKSM(NPI),  IUPOP(NPI),  NAMES(NPI),  IPSWICH(NPI),
     $            POPSYM(NPI), KAPNO(NPI),  POPMSS(NPI), TNAMES(NPI),
     $            KLABPI(NPI), NLABPI(NPI), BLABPI(NPI)
C
      common      /POPS01/ NPOPS,MAXPOPL,LENT,LENPBL,MRTPM,MRTPA,ICKSM
      common      /POPS02/ POPMSS
      common      /POPS03/ LZOQ
      common      /POPS04/ MRTP
      common      /POPS05/ LENPOP
      common      /POPS06/ LIMPOP
      common      /POPS07/ NAMES
      common      /POPS08/ TNAMES
      common      /POPS09/ NAMKNT
      common      /POPS10/ IUPOP
      common      /POPS11/ IBLAD
      common      /POPS12/ IPSWICH
      common      /POPS13/ POPSYM
      common      /POPS14/ KAPNO
      common      /POPS15/ KLABPI
      common      /POPS16/ NLABPI
      common      /POPS17/ BLABPI
C
C     Population Data Blocks parameters and data.
      equivalence (LZOQ( 4),LLPOPN)
      equivalence (LZOQ( 3),LLPOPK)
      equivalence (LZOQ( 5),LLBD  )
C     !DASH
C     !EJECT
      external LUNA, POPIO, ULLA, THAMOS, PETRA, PUPPET, FRAGA, HI, BYE
C
      dimension X(*), W(*)
C
C               XPBL(Lenpbl), ABDEL(N)
      dimension XPBL(*),      ABDEL(*)
C
      call HI ('POPUP')
C     !BEG
      call LUNA          (X, QELSM, ABD, ABDEL)
      if(IUPOP(1).gt.0) then
C----   Hydrogen
        call POPIO       ('READ', 1, XPBL)
        call ULLA        (X(JJXND), X(JJNK), XPBL(LLPOPN),
     $                    XPBL(LLPOPK), N, NAMES(1), NAMKNT(1),
     $                    X(JJHND), X(JJXNE), X(JJTE), ABDEL,
     $                    LIMPOP(1), X(JJBDI), XPBL(LLBD), IPSWICH(1),
     $                    X(JJBTL), W)
        call POPIO       ('WRITE', jummy, XPBL)
        call THAMOS      (XPBL, X(JJH1))
        LENPOP(1) = LIMPOP(1)
        call PUPPET      (ICKSM(1), N, LIMPOP(1), XPBL(LLPOPK),
     $                    XPBL(LLPOPN), 1, XPBL(LLBD))
      else
C----   Others
        do 100 I = 2,NPOPS
          if(IUPOP(I).gt.0) then
            call POPIO   ('READ', I, XPBL)
            call PETRA   (X(JJXND), XPBL(LLPOPN), X(JJNK),
     $                    XPBL(LLPOPK), N, NAMES(I), NAMKNT(I),
     $                    X(JJHND), X(JJXNE), X(JJTE), ABDEL, I,
     $                    LIMPOP(I), X(JJBDI), XPBL(LLBD), IPSWICH(I),
     $                    X(JJBTL))
            call POPIO   ('WRITE', jummy, XPBL)
            LENPOP(I) = LIMPOP(I)
            call PUPPET  (ICKSM(I), N, LIMPOP(I), XPBL(LLPOPK),
     $                    XPBL(LLPOPN), 1, XPBL(LLBD))
            if(I.eq.5) then
C----         This is HeII --- update charged particle density
              call FRAGA (X, XPBL)
            end if
          end if
  100   continue
      end if
C     !END
      call BYE ('POPUP')
C
      return
      end
