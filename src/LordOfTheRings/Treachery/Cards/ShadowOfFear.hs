module LordOfTheRings.Treachery.Cards.ShadowOfFear (shadowOfFear) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ShadowOfFear = ShadowOfFear TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, Entity)

shadowOfFear :: TreacheryCard ShadowOfFear
shadowOfFear = treachery ShadowOfFear Cards.shadowOfFear

-- | Counts as a Condition attachment with the text: "Limit 1 per hero. Attached hero
-- cannot exhaust or ready and its text box is treated as if it were blank."
-- ponytail: the blank text box is its abilities not triggering, as Sacked's
instance HasModifiersFor ShadowOfFear where
  getModifiersFor (ShadowOfFear a) = withHost a \host ->
    modified a host [CannotExhaust, CannotReady, CannotTriggerAbilities]

-- | Action: Pay 3 resources from attached hero's pool to discard this card.
instance HasAbilities ShadowOfFear where
  getAbilities (ShadowOfFear a) = case a.placement of
    AttachedTo host -> [mkAbility a 1 $ ActionAbility (ResourceCost host 3)]
    _ -> []

-- | When Revealed: The first player attaches Shadow of Fear to one of his heroes.
instance RunMessage ShadowOfFear where
  runMessage msg t@(ShadowOfFear attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      heroes <- select (HeroControlledBy $ PlayerWithId lead)
      taken <- hostsOfTreachery (toCardCode Cards.shadowOfFear)
      chooseOrRunOneM lead do
        questionLabeled "shadowOfFear.attach"
        causedBy attrs
        targets (filter (\h -> not $ any (isTarget h) taken) heroes) \h -> attachTreachery attrs.id h
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discard attrs
      pure t
    _ -> ShadowOfFear <$> runMessage msg attrs
