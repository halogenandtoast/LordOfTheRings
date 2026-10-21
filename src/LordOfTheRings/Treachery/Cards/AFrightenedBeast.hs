module LordOfTheRings.Treachery.Cards.AFrightenedBeast (aFrightenedBeast) where

import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Trait (Trait (Creature))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AFrightenedBeast = AFrightenedBeast TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aFrightenedBeast :: TreacheryCard AFrightenedBeast
aFrightenedBeast = treachery AFrightenedBeast Cards.aFrightenedBeast

-- | When Revealed: Each player raises his threat by the total threat of all cards in the
-- staging area. Any player may choose to discard from play 1 Creature ally card he
-- controls to cancel this effect.
instance RunMessage AFrightenedBeast where
  runMessage msg t@(AFrightenedBeast attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      threat <- getStagingThreat
      players <- getRemainingPlayers
      creatures <- for players \p -> (p,) <$> select (AllyControlledBy (PlayerWithId p) <> AllyWithTrait Creature)
      offer attrs threat [c | c@(_, allies) <- creatures, notNull allies]
      pure t
    _ -> AFrightenedBeast <$> runMessage msg attrs

-- | Each player with a Creature ally in turn may cancel the effect; if none does, it
-- resolves.
offer :: (ReverseQueue m) => TreacheryAttrs -> Int -> [(PlayerId, [AllyId])] -> m ()
offer attrs threat = \case
  [] -> eachPlayer \p -> raiseThreat p threat
  (p, allies) : rest -> chooseOneM p do
    questionLabeled "aFrightenedBeast.cancel"
    causedBy attrs
    targets allies discard
    labeled "aFrightenedBeast.allow" $ offer attrs threat rest
