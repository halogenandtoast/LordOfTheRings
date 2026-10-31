module LordOfTheRings.Treachery.Cards.PoisonDarts (poisonDarts) where

import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PoisonDarts = PoisonDarts TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

poisonDarts :: TreacheryCard PoisonDarts
poisonDarts = treachery PoisonDarts Cards.poisonDarts

-- | Counts as a Weapon attachment with the text: "Attached enemy gets +2 attack and gains
-- archery 2."
instance HasModifiersFor PoisonDarts where
  getModifiersFor (PoisonDarts a) = case a.placement of
    AttachedTo (EnemyTarget eid) -> modified a eid [AttackModifier 2, GainKeyword (Archery 2)]
    _ -> pure ()

-- | Surge.
-- When Revealed: Attach to the enemy in play with the highest engagement cost: the first
-- player picks among those tied.
instance RunMessage PoisonDarts where
  runMessage msg t@(PoisonDarts attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- traverse (\e -> (e,) <$> getEngagementCost e) =<< select AnyEnemy
      let highest = foldr (max . snd) 0 enemies
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "poisonDarts.attach"
        causedBy attrs
        targets [e | (e, c) <- enemies, c == highest] $ attachTreachery attrs.treacheryId
      pure t
    _ -> PoisonDarts <$> runMessage msg attrs
