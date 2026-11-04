module LordOfTheRings.Treachery.Cards.TerribleFever (terribleFever) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TerribleFever = TerribleFever TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

terribleFever :: TreacheryCard TerribleFever
terribleFever = treachery TerribleFever Cards.terribleFever

-- | Counts as a Condition attachment with the text: "Forced: At the end of the round, deal
-- 1 damage to attached character."
instance HasAbilities TerribleFever where
  getAbilities (TerribleFever a) = case a.placement of
    AttachedTo _ -> [mkAbility a 1 $ forced RoundEndsWindow]
    _ -> []

{- | When Revealed: Remove a hero from the quest and attach Terrible Fever to that hero.
The first player chooses a hero committed to the quest, or, with none committed (revealed
outside the quest), any hero.
Shadow: Defending character cannot ready until the end of the round.
-}
instance RunMessage TerribleFever where
  runMessage msg t@(TerribleFever attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      committed <- select CommittedHero
      heroes <- if null committed then select AnyHero else pure committed
      chooseOrRunOneM lead do
        questionLabeled "terribleFever.attach"
        causedBy attrs
        targets heroes \h -> do
          removeFromQuest h
          attachTreachery attrs.treacheryId h
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs \h -> dealDamage attrs h 1
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> roundModifier attrs d CannotReady
      pure t
    _ -> TerribleFever <$> runMessage msg attrs
