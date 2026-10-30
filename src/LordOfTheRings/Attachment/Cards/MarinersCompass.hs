module LordOfTheRings.Attachment.Cards.MarinersCompass (marinersCompass) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (Card (EncounterCard), CardType (LocationType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Phase (Phase (TravelPhase))
import LordOfTheRings.Sphere (Sphere (Leadership))
import LordOfTheRings.Trait (Trait (Scout))

newtype MarinersCompass = MarinersCompass AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a leadership or a Scout character.
marinersCompass :: AttachmentCard MarinersCompass
marinersCompass =
  attachment MarinersCompass Cards.marinersCompass
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithSphere Leadership, HeroWithTrait Scout])
          (AllyMatchAny [AllyWithSphere Leadership, AllyWithTrait Scout])
      )

-- | Response: At the beginning of the travel phase, exhaust Mariner's Compass and attached
-- character to search the top 5 cards of the encounter deck for a location. Switch that
-- location with a location in the staging area. Shuffle the encounter deck.
instance HasAbilities MarinersCompass where
  getAbilities (MarinersCompass a) =
    [ restricted a 1 (exists LocationInStagingArea)
        $ ResponseAbility (PhaseBeginsWindow After TravelPhase) (Costs [exhaustCost a, ExhaustCost host])
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage MarinersCompass where
  runMessage msg s@(MarinersCompass a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      top <- take 5 . (.encounterDeck) . toAttrs <$> getsGame (.scenario)
      staged <- select LocationInStagingArea
      chooseOneM pid do
        questionLabeled "marinersCompass.search"
        causedBy a
        showing (map EncounterCard top)
        targets (filter (cardMatches $ CardWithType LocationType) top) \c -> chooseOneM pid do
          questionLabeled "marinersCompass.switch"
          causedBy a
          targets staged \lid -> do
            removeFromEncounterDeck c
            shuffleIntoEncounterDeck lid
            addToStaging c
      shuffleEncounterDeck
      pure s
    _ -> MarinersCompass <$> runMessage msg a
