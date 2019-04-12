      subroutine NADA
     $(N,TE,XNE,PGS,XION,RHEAB,HEK,HE1,HE2K,HE21,DEE,DEEL,RFHEA,
     $ FZION,ZION)
C
C     Rudolf Loeser, 1998 Oct 29
C---- Prints the d-coefficients, for DANA.
C     !DASH
      save
C     !DASH
      real*8 DEE, DEEL, FZION, HE1, HE21, HE2K, HEK, PGS, RFHEA, RHEAB,
     $       TE, XION, XNE, ZION
      integer I, IE, IPDEE, IS, J, K, LUEO, N, NEFDF
      logical KILROY
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(163),NEFDF)
      equivalence (KZQ(165),IPDEE)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  ACTON, VECOUT, LINER, HI, BYE
      intrinsic min
C
C               TE(N), PGS(N), XNE(N), DEE(4,5,N), DEEL(4,5,N), HEK(N),
      dimension TE(*), PGS(*), XNE(*), DEE(4,5,*), DEEL(*),     HEK(*),
C
C               ZION(N), RHEAB(N), XION(N), HE1(N), HE2K(N), HE21(N)
     $          ZION(*), RHEAB(*), XION(*), HE1(*), HE2K(*), HE21(*)
C
      data KILROY /.true./
C
      call HI ('NADA')
C     !BEG
      if(KILROY) then
C----   Do any of this only once in a run
        KILROY = .false.
C
        call LINER     (3, LUEO)
        if(IPDEE.le.0) then
          write (LUEO,100)
  100     format(' ','For a complete printout of d-coefficients, ',
     $               'set IPDEE = 1.')
        else
C     !EJECT
C----     Print
          write (LUEO,101) FZION,RFHEA
  101     format(' ','Complete printout of d-coefficients.'//
     $           ' ','To omit this printout of d-coefficients, set ',
     $               'IPDEE = 0 (the default).'//
     $           ' ','The d-values printed are those computed by the ',
     $               'procedures from the subroutine by Juan Fontenla.'/
     $           ' ','***** Note that these values are multiplied by ',
     $               'ZION, FZION, and RFHEA before use.'/
     $           ' ','(FZION =',1PE16.8,', RFHEA =',E16.8')')
          call VECOUT  (LUEO, ZION, N, 'ZION')
          call LINER   (2, LUEO)
C
          IE = 0
  102     continue
            IS = IE+1
            IE = min((IE+10),N)
            call LINER (2, LUEO)
            write (LUEO,103) (K,K=IS,IE)
  103       format(' ','depth #',10I12)
C
  104       format(' ',A7,1P10E12.4)
            write (LUEO,104)   '     TE', (TE   (K),K=IS,IE)
            write (LUEO,104)   '    PGS', (PGS  (K),K=IS,IE)
            if(NEFDF.eq.2) then
              write (LUEO,104) '     NE', (XNE  (K),K=IS,IE)
            end if
            write (LUEO,104)   '  RHEAB', (RHEAB(K),K=IS,IE)
            write (LUEO,104)   '   XION', (XION (K),K=IS,IE)
            write (LUEO,104)   '    HEK', (HEK  (K),K=IS,IE)
            write (LUEO,104)   '    HE1', (HE1  (K),K=IS,IE)
            write (LUEO,104)   '   HE2K', (HE2K (K),K=IS,IE)
            write (LUEO,104)   '   HE21', (HE21 (K),K=IS,IE)
            call LINER (1, LUEO)
C
            do 107 J = 1,5
              do 106 I = 1,4
                write (LUEO,105) I,J,(DEE(I,J,K),K=IS,IE)
  105           format(' ',' d(',I1,',',I1,')',1P10E12.4)
  106         continue
  107       continue
          if(IE.lt.N) goto 102
C
        end if
C
C----   Plot
        call ACTON     (N, DEE, DEEL, 'computed')
C
      end if
C     !END
      call BYE ('NADA')
C
      return
      end
