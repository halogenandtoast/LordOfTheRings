{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Attachment (module LordOfTheRings.Attachment, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Attachment.Cards.AncientMathom
import LordOfTheRings.Attachment.Cards.SpearOfTheMark
import LordOfTheRings.Attachment.Cards.SteedOfTheMark
import LordOfTheRings.Attachment.Cards.RohanWarhorse
import LordOfTheRings.Attachment.Cards.Firefoot
import LordOfTheRings.Attachment.Cards.TheRedArrow
import LordOfTheRings.Attachment.Cards.WarAxe
import LordOfTheRings.Attachment.Cards.HornOfTheMark
import LordOfTheRings.Attachment.Cards.BladeOfGondolin
import LordOfTheRings.Attachment.Cards.CelebriansStone
import LordOfTheRings.Attachment.Cards.CitadelPlate
import LordOfTheRings.Attachment.Cards.DarkKnowledge
import LordOfTheRings.Attachment.Cards.DunedainMark
import LordOfTheRings.Attachment.Cards.DwarvenAxe
import LordOfTheRings.Attachment.Cards.ForestSnare
import LordOfTheRings.Attachment.Cards.HornOfGondor
import LordOfTheRings.Attachment.Cards.PowerInTheEarth
import LordOfTheRings.Attachment.Cards.ProtectorOfLorien
import LordOfTheRings.Attachment.Cards.SelfPreservation
import LordOfTheRings.Attachment.Cards.StewardOfGondor
import LordOfTheRings.Attachment.Cards.TheFavorOfTheLady
import LordOfTheRings.Attachment.Cards.UnexpectedCourage
import LordOfTheRings.Attachment.Cards.SongOfKings
import LordOfTheRings.Attachment.Cards.DunedainWarning
import LordOfTheRings.Attachment.Cards.BornAloft
import LordOfTheRings.Attachment.Cards.NorAmIAStranger
import LordOfTheRings.Attachment.Cards.ABurningBrand
import LordOfTheRings.Attachment.Cards.SongOfWisdom
import LordOfTheRings.Attachment.Cards.DunedainQuest
import LordOfTheRings.Attachment.Cards.SongOfTravel
import LordOfTheRings.Attachment.Cards.DunedainCache
import LordOfTheRings.Attachment.Cards.SongOfMocking
import LordOfTheRings.Attachment.Cards.FastHitch
import LordOfTheRings.Attachment.Cards.SongOfBattle
import LordOfTheRings.Attachment.Cards.DunedainSignal
import LordOfTheRings.Attachment.Cards.SupportOfTheEagles
import LordOfTheRings.Attachment.Cards.Vilya
import LordOfTheRings.Attachment.Cards.Nenya
import LordOfTheRings.Attachment.Cards.BloodOfNumenor
import LordOfTheRings.Attachment.Cards.GondorianFire
import LordOfTheRings.Attachment.Cards.MirrorOfGaladriel
import LordOfTheRings.Attachment.Cards.OLorien
import LordOfTheRings.Attachment.Cards.Lembas
import LordOfTheRings.Attachment.Cards.BowOfTheGaladhrim
import LordOfTheRings.Attachment.Cards.Wingfoot
import LordOfTheRings.Attachment.Cards.NarvisBelt
import LordOfTheRings.Attachment.Cards.LegacyOfDurin
import LordOfTheRings.Attachment.Cards.HealingHerbs
import LordOfTheRings.Attachment.Cards.Cram
import LordOfTheRings.Attachment.Cards.ThrorsMap
import LordOfTheRings.Attachment.Cards.KingUnderTheMountain
import LordOfTheRings.Attachment.Cards.DwarfPipe
import LordOfTheRings.Attachment.Cards.MagicRing
import LordOfTheRings.Attachment.Cards.PathOfNeed
import LordOfTheRings.Attachment.Cards.RangerSpikes
import LordOfTheRings.Attachment.Cards.SpearOfTheCitadel
import LordOfTheRings.Attachment.Cards.IthilienPit
import LordOfTheRings.Attachment.Cards.GondorianShield
import LordOfTheRings.Attachment.Cards.ValiantSword
import LordOfTheRings.Attachment.Cards.VisionaryLeadership
import LordOfTheRings.Attachment.Cards.HeirOfMardil
import LordOfTheRings.Attachment.Cards.CaptainOfGondor
import LordOfTheRings.Attachment.Cards.DwarrowdelfAxe
import LordOfTheRings.Attachment.Cards.BootsFromErebor
import LordOfTheRings.Attachment.Cards.KeepingCount
import LordOfTheRings.Attachment.Cards.GoodMeal
import LordOfTheRings.Attachment.Cards.RivendellBlade
import LordOfTheRings.Attachment.Cards.SongOfEarendil
import LordOfTheRings.Attachment.Cards.SwordThatWasBroken
import LordOfTheRings.Attachment.Cards.RivendellBow
import LordOfTheRings.Attachment.Cards.Resourceful
import LordOfTheRings.Attachment.Cards.Valor
import LordOfTheRings.Attachment.Cards.IarionsPendant
import LordOfTheRings.Attachment.Cards.Cunning
import LordOfTheRings.Attachment.Cards.Hardy
import LordOfTheRings.Attachment.Cards.Resolute
import LordOfTheRings.Attachment.Cards.Ruthless
import LordOfTheRings.Attachment.Cards.Stalwart
import LordOfTheRings.Attachment.Cards.AmarthiulsCourage
import LordOfTheRings.Attachment.Cards.ChieftainsBrooch
import LordOfTheRings.Attachment.Cards.MailOfEarnur
import LordOfTheRings.Attachment.Cards.Silverthorn
import LordOfTheRings.Attachment.Cards.DaechanarsBrand
import LordOfTheRings.Attachment.Cards.HeirloomOfIarchon
import LordOfTheRings.Attachment.Cards.OrdersFromAngmar
import LordOfTheRings.Attachment.Cards.RaimentOfTheSecondAge
import LordOfTheRings.Attachment.Cards.AppointedByFate
import LordOfTheRings.Attachment.Cards.RingMail
import LordOfTheRings.Attachment.Cards.EverMyHeartRises
import LordOfTheRings.Attachment.Cards.LoveOfTales
import LordOfTheRings.Attachment.Cards.LightOfValinor
import LordOfTheRings.Attachment.Cards.Asfaloth
import LordOfTheRings.Attachment.Cards.HardyLeadership
import LordOfTheRings.Attachment.Cards.Miruvor
import LordOfTheRings.Attachment.Cards.RingOfBarahir
import LordOfTheRings.Attachment.Cards.MightyProwess
import LordOfTheRings.Attachment.Cards.LordOfMorthond
import LordOfTheRings.Attachment.Cards.BookOfEldacar
import LordOfTheRings.Attachment.Cards.SwordOfMorthond
import LordOfTheRings.Attachment.Cards.MapOfEarnil
import LordOfTheRings.Attachment.Cards.RangerBow
import LordOfTheRings.Attachment.Cards.Palantir
import LordOfTheRings.Attachment.Cards.TomeOfAtanatar
import LordOfTheRings.Attachment.Cards.PoisonedStakes
import LordOfTheRings.Attachment.Cards.ScrollOfIsildur
import LordOfTheRings.Attachment.Cards.KeysOfOrthanc
import LordOfTheRings.Attachment.Cards.SilverLamp
import LordOfTheRings.Attachment.Cards.ElvenMail
import LordOfTheRings.Attachment.Cards.CloakOfLorien
import LordOfTheRings.Attachment.Cards.LeafBrooch
import LordOfTheRings.Attachment.Cards.DefenderOfTheWest
import LordOfTheRings.Attachment.Cards.TheFallOfGilGalad
import LordOfTheRings.Attachment.Cards.WardenOfArnor
import LordOfTheRings.Attachment.Cards.TheDaysRising
import LordOfTheRings.Attachment.Cards.TheLongDefeat
import LordOfTheRings.Attachment.Cards.FavorOfTheValar
import LordOfTheRings.Attachment.Cards.SwordOfNumenor
import LordOfTheRings.Attachment.Cards.WeatherStainedCloak
import LordOfTheRings.Attachment.Cards.SwordThain
import LordOfTheRings.Attachment.Cards.RangerProvisions
import LordOfTheRings.Attachment.Cards.SteedOfImladris
import LordOfTheRings.Attachment.Cards.ElvenSpear
import LordOfTheRings.Attachment.Cards.SilverHarp
import LordOfTheRings.Attachment.Cards.ElfFriend
import LordOfTheRings.Attachment.Cards.HeirOfValandil
import LordOfTheRings.Attachment.Cards.Athelas
import LordOfTheRings.Attachment.Cards.SecretVigil
import LordOfTheRings.Attachment.Cards.StarBrooch
import LordOfTheRings.Attachment.Cards.RavenWingedHelm
import LordOfTheRings.Attachment.Cards.HobbitPony
import LordOfTheRings.Attachment.Cards.GrapplingHook
import LordOfTheRings.Attachment.Cards.MarinersCompass
import LordOfTheRings.Attachment.Cards.ExplorersAlmanac
import LordOfTheRings.Attachment.Cards.ToTheSeaToTheSea
import LordOfTheRings.Attachment.Cards.Narya
import LordOfTheRings.Attachment.Cards.RodOfTheSteward
import LordOfTheRings.Attachment.Cards.InServiceOfTheSteward
import LordOfTheRings.Attachment.Cards.RaimentOfWar
import LordOfTheRings.Attachment.Cards.GuardedCeaselessly
import LordOfTheRings.Attachment.Cards.ArmoredDestrier
import LordOfTheRings.Attachment.Cards.EntanglingNets
import LordOfTheRings.Attachment.Cards.DunedainRemedy
import LordOfTheRings.Attachment.Cards.Hithlain
import LordOfTheRings.Attachment.Cards.Strider
import LordOfTheRings.Attachment.Cards.RuneMaster
import LordOfTheRings.Attachment.Cards.VigilantGuard
import LordOfTheRings.Attachment.Cards.Windfola
import LordOfTheRings.Attachment.Cards.RangerSpear
import LordOfTheRings.Attachment.Cards.PrinceOfDolAmroth
import LordOfTheRings.Attachment.Cards.ArmorPlating
import LordOfTheRings.Attachment.Cards.Ballista
import LordOfTheRings.Attachment.Cards.EnlargedQuarters
import LordOfTheRings.Attachment.Cards.ExtraSails
import LordOfTheRings.Attachment.Cards.Spyglass
import LordOfTheRings.Attachment.Cards.BlackKey
import LordOfTheRings.Attachment.Cards.ParryingCutlass
import LordOfTheRings.Attachment.Cards.ThrowingAxe
import LordOfTheRings.Attachment.Cards.CirdansBlessing
import LordOfTheRings.Attachment.Cards.HaradrimSpear
import LordOfTheRings.Attachment.Cards.FiresideSong
import LordOfTheRings.Attachment.Cards.DoughtyRanger
import LordOfTheRings.Attachment.Cards.SongOfHope
import LordOfTheRings.Attachment.Cards.FearlessScout
import LordOfTheRings.Attachment.Cards.DunedainPipe
import LordOfTheRings.Attachment.Cards.DiligentNoble
import LordOfTheRings.Attachment.Cards.LegacyBlade
import LordOfTheRings.Attachment.Cards.Followed
import LordOfTheRings.Attachment.Runner ()
import LordOfTheRings.Attachment.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored attachment is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Attachment where
  parseJSON v = do
    attrs <- parseJSON @AttachmentAttrs v
    pure $ overAttrs (const attrs) $ lookupAttachment (attachmentCardCode attrs) (attachmentCardId attrs) (attachmentId attrs) (attachmentOwner attrs)

instance RunMessage Attachment where
  runMessage msg (Attachment a) = Attachment <$> runMessage msg a

createAttachment :: PlayerCard -> AttachmentId -> PlayerId -> Attachment
createAttachment card ident pid = lookupAttachment (toCardCode card) (pcId card) ident pid

lookupAttachment :: (HasCallStack) => CardCode -> CardId -> AttachmentId -> PlayerId -> Attachment
lookupAttachment cardCode cardId ident pid = case Map.lookup cardCode allAttachments of
  Just (SomeAttachmentCard builder) -> Attachment $ cbCardBuilder builder cardId (ident, pid)
  Nothing -> error $ "Unknown attachment: " <> show cardCode

newAttachment :: (MonadRandom m) => PlayerId -> PlayerCard -> m Attachment
newAttachment pid card = do
  ident <- getRandom
  pure $ createAttachment card ident pid

allAttachments :: Map CardCode SomeAttachmentCard
allAttachments =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeAttachmentCard builder) <-
        [ SomeAttachmentCard stewardOfGondor
        , SomeAttachmentCard celebriansStone
        , SomeAttachmentCard vilya
        , SomeAttachmentCard nenya
        , SomeAttachmentCard bloodOfNumenor
        , SomeAttachmentCard gondorianFire
        , SomeAttachmentCard mirrorOfGaladriel
        , SomeAttachmentCard oLorien
        , SomeAttachmentCard lembas
        , SomeAttachmentCard bowOfTheGaladhrim
        , SomeAttachmentCard wingfoot
        , SomeAttachmentCard bladeOfGondolin
        , SomeAttachmentCard citadelPlate
        , SomeAttachmentCard dwarvenAxe
        , SomeAttachmentCard hornOfGondor
        , SomeAttachmentCard theFavorOfTheLady
        , SomeAttachmentCard powerInTheEarth
        , SomeAttachmentCard unexpectedCourage
        , SomeAttachmentCard forestSnare
        , SomeAttachmentCard protectorOfLorien
        , SomeAttachmentCard darkKnowledge
        , SomeAttachmentCard selfPreservation
        , SomeAttachmentCard dunedainMark
        , SomeAttachmentCard songOfKings
        , SomeAttachmentCard dunedainWarning
        , SomeAttachmentCard bornAloft
        , SomeAttachmentCard norAmIAStranger
        , SomeAttachmentCard aBurningBrand
        , SomeAttachmentCard songOfWisdom
        , SomeAttachmentCard dunedainQuest
        , SomeAttachmentCard songOfTravel
        , SomeAttachmentCard dunedainCache
        , SomeAttachmentCard songOfMocking
        , SomeAttachmentCard fastHitch
        , SomeAttachmentCard songOfBattle
        , SomeAttachmentCard dunedainSignal
        , SomeAttachmentCard supportOfTheEagles
        , SomeAttachmentCard ancientMathom
        , SomeAttachmentCard spearOfTheMark
        , SomeAttachmentCard steedOfTheMark
        , SomeAttachmentCard rohanWarhorse
        , SomeAttachmentCard firefoot
        , SomeAttachmentCard theRedArrow
        , SomeAttachmentCard warAxe
        , SomeAttachmentCard hornOfTheMark
        , SomeAttachmentCard narvisBelt
        , SomeAttachmentCard legacyOfDurin
        , SomeAttachmentCard healingHerbs
        , SomeAttachmentCard cram
        , SomeAttachmentCard throrsMap
        , SomeAttachmentCard kingUnderTheMountain
        , SomeAttachmentCard dwarfPipe
        , SomeAttachmentCard magicRing
        , SomeAttachmentCard pathOfNeed
        , SomeAttachmentCard rangerSpikes
        , SomeAttachmentCard spearOfTheCitadel
        , SomeAttachmentCard ithilienPit
        , SomeAttachmentCard gondorianShield
        , SomeAttachmentCard valiantSword
        , SomeAttachmentCard visionaryLeadership
        , SomeAttachmentCard heirOfMardil
        , SomeAttachmentCard captainOfGondor
        , SomeAttachmentCard dwarrowdelfAxe
        , SomeAttachmentCard bootsFromErebor
        , SomeAttachmentCard keepingCount
        , SomeAttachmentCard goodMeal
        , SomeAttachmentCard rivendellBlade
        , SomeAttachmentCard songOfEarendil
        , SomeAttachmentCard swordThatWasBroken
        , SomeAttachmentCard rivendellBow
        , SomeAttachmentCard resourceful
        , SomeAttachmentCard valor
        , SomeAttachmentCard iarionsPendant
        , SomeAttachmentCard cunning
        , SomeAttachmentCard hardy
        , SomeAttachmentCard resolute
        , SomeAttachmentCard ruthless
        , SomeAttachmentCard stalwart
        , SomeAttachmentCard amarthiulsCourage
        , SomeAttachmentCard chieftainsBrooch
        , SomeAttachmentCard mailOfEarnur
        , SomeAttachmentCard silverthorn
        , SomeAttachmentCard daechanarsBrand
        , SomeAttachmentCard heirloomOfIarchon
        , SomeAttachmentCard ordersFromAngmar
        , SomeAttachmentCard raimentOfTheSecondAge
        , SomeAttachmentCard appointedByFate
        , SomeAttachmentCard ringMail
        , SomeAttachmentCard everMyHeartRises
        , SomeAttachmentCard loveOfTales
        , SomeAttachmentCard lightOfValinor
        , SomeAttachmentCard asfaloth
        , SomeAttachmentCard hardyLeadership
        , SomeAttachmentCard miruvor
        , SomeAttachmentCard ringOfBarahir
        , SomeAttachmentCard mightyProwess
        , SomeAttachmentCard lordOfMorthond
        , SomeAttachmentCard bookOfEldacar
        , SomeAttachmentCard swordOfMorthond
        , SomeAttachmentCard mapOfEarnil
        , SomeAttachmentCard rangerBow
        , SomeAttachmentCard palantir
        , SomeAttachmentCard tomeOfAtanatar
        , SomeAttachmentCard poisonedStakes
        , SomeAttachmentCard scrollOfIsildur
        , SomeAttachmentCard keysOfOrthanc
        , SomeAttachmentCard silverLamp
        , SomeAttachmentCard elvenMail
        , SomeAttachmentCard cloakOfLorien
        , SomeAttachmentCard leafBrooch
        , SomeAttachmentCard defenderOfTheWest
        , SomeAttachmentCard theFallOfGilGalad
        , SomeAttachmentCard wardenOfArnor
        , SomeAttachmentCard theDaysRising
        , SomeAttachmentCard theLongDefeat
        , SomeAttachmentCard favorOfTheValar
        , SomeAttachmentCard swordOfNumenor
        , SomeAttachmentCard weatherStainedCloak
        , SomeAttachmentCard swordThain
        , SomeAttachmentCard rangerProvisions
        , SomeAttachmentCard steedOfImladris
        , SomeAttachmentCard elvenSpear
        , SomeAttachmentCard silverHarp
        , SomeAttachmentCard elfFriend
        , SomeAttachmentCard heirOfValandil
        , SomeAttachmentCard athelas
        , SomeAttachmentCard secretVigil
        , SomeAttachmentCard starBrooch
        , SomeAttachmentCard ravenWingedHelm
        , SomeAttachmentCard hobbitPony
        , SomeAttachmentCard grapplingHook
        , SomeAttachmentCard marinersCompass
        , SomeAttachmentCard explorersAlmanac
        , SomeAttachmentCard toTheSeaToTheSea
        , SomeAttachmentCard narya
        , SomeAttachmentCard rodOfTheSteward
        , SomeAttachmentCard inServiceOfTheSteward
        , SomeAttachmentCard raimentOfWar
        , SomeAttachmentCard guardedCeaselessly
        , SomeAttachmentCard armoredDestrier
        , SomeAttachmentCard entanglingNets
        , SomeAttachmentCard dunedainRemedy
        , SomeAttachmentCard hithlain
        , SomeAttachmentCard strider
        , SomeAttachmentCard runeMaster
        , SomeAttachmentCard vigilantGuard
        , SomeAttachmentCard windfola
        , SomeAttachmentCard rangerSpear
        , SomeAttachmentCard princeOfDolAmroth
        , SomeAttachmentCard armorPlating
        , SomeAttachmentCard armorPlatingUpgraded
        , SomeAttachmentCard ballista
        , SomeAttachmentCard ballistaUpgraded
        , SomeAttachmentCard enlargedQuarters
        , SomeAttachmentCard enlargedQuartersUpgraded
        , SomeAttachmentCard extraSails
        , SomeAttachmentCard extraSailsUpgraded
        , SomeAttachmentCard spyglass
        , SomeAttachmentCard spyglassUpgraded
        , SomeAttachmentCard blackKey
        , SomeAttachmentCard blackKeyUpgraded
        , SomeAttachmentCard parryingCutlass
        , SomeAttachmentCard parryingCutlassUpgraded
        , SomeAttachmentCard throwingAxe
        , SomeAttachmentCard throwingAxeUpgraded
        , SomeAttachmentCard cirdansBlessingLeadership
        , SomeAttachmentCard cirdansBlessingTactics
        , SomeAttachmentCard cirdansBlessingSpirit
        , SomeAttachmentCard cirdansBlessingLore
        , SomeAttachmentCard haradrimSpear
        , SomeAttachmentCard firesideSong
        , SomeAttachmentCard doughtyRanger
        , SomeAttachmentCard songOfHope
        , SomeAttachmentCard fearlessScout
        , SomeAttachmentCard dunedainPipe
        , SomeAttachmentCard diligentNoble
        , SomeAttachmentCard legacyBlade
        , SomeAttachmentCard followed
        ]
    ]
