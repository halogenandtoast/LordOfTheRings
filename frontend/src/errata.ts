// The cards with an errata'd image (FAQ 1.9, and the later errata whose images we have),
// by the image name of the printed card. A player's card shows it while their deck plays
// with errata; an encounter card always does.
const printedToErrata: Record<string, string> = Object.fromEntries(
  [
    // player cards
    '01012', '01034', '01036', '01042', '01043', '01049', '01070', '02033', '02095', '03003',
    '03009', '04036', '04061', '04076', '04081', '04085', '04129', '04132', '05012', '05016',
    '06036', '06107', '08006', '08092', '131003', '131013', '132011', '132017', '17135',
    // encounter and quest cards
    '01091', '01102', '01123', '02017', '04027', '05029', '05061', '06106', '06117-b', '06123',
    '06157', '131042', '131043', '132045', '132068-b', '143043', '144067',
  ].map((image) => [image, `${image}-errata`]),
)
// the second errata, adding "(Limit +4 attack.)"
printedToErrata['04079'] = '04079-errata-2'

/** The image to show for a card's face, errata'd when it plays with errata. */
export const errataImage = (image: string, errata: boolean) => (errata && printedToErrata[image]) || image
