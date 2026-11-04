module LordOfTheRings.Attachment.Cards.PalantirOfOrthanc (palantirOfOrthanc) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Gift (spendBoon)
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Sphere (Sphere (Fellowship))

newtype PalantirOfOrthanc = PalantirOfOrthanc AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Treason of Saruman. Attach to [fellowship] Aragorn.
palantirOfOrthanc :: AttachmentCard PalantirOfOrthanc
palantirOfOrthanc =
  attachment PalantirOfOrthanc Cards.palantirOfOrthanc
    & attachTo (AttachToHero $ HeroWithTitle "Aragorn" <> HeroWithSphere Fellowship)

-- | Quest Action: Exhaust Aragorn and Palantír of Orthanc to discard the top card of the
-- encounter deck. If Aragorn's [willpower] is greater than the discarded card's [threat],
-- you may add Palantír of Orthanc to the victory display and remove it from the campaign
-- pool to choose a player. That player reveals 1 fewer encounter card this phase (to a
-- minimum of 0).
instance HasAbilities PalantirOfOrthanc where
  getAbilities (PalantirOfOrthanc a) =
    [ restricted a 1 (DuringPhase QuestPhase) $ ActionAbility (Costs [ExhaustCost host, exhaustCost a])
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage PalantirOfOrthanc where
  runMessage msg s@(PalantirOfOrthanc a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> discardTopOfEncounterDeckFor a host 1
      pure s
    DiscardedTopOfEncounterDeck (isSource a -> True) aragorn cards -> do
      willpower <- getWillpower aragorn
      let threat = sum [fromMaybe 0 (cdThreat $ toCardDef c) | c <- cards]
      when (notNull cards && willpower > threat) $ chooseOneM a.controller do
        questionLabeled "palantirOfOrthanc.use"
        causedBy a
        labeled "palantirOfOrthanc.spend" do
          spendBoon a
          choosePlayerM a.controller a \p -> phaseModifier a (PlayerTarget p) (FewerStagingReveals 1)
        labeled "palantirOfOrthanc.keep" (pure ())
      pure s
    _ -> PalantirOfOrthanc <$> runMessage msg a
