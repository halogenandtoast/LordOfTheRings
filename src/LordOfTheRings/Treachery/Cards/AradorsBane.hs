module LordOfTheRings.Treachery.Cards.AradorsBane (aradorsBane) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Trait (Trait (Giant, Troll))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AradorsBane = AradorsBane TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aradorsBane :: TreacheryCard AradorsBane
aradorsBane = treachery AradorsBane Cards.aradorsBane

-- | When Revealed: Starting with the first player, each player must either engage an enemy
-- in the staging area with a higher engagement cost than his threat, or search the
-- encounter deck and discard pile for a Troll or Giant enemy and add it to the staging
-- area. Deal each enemy that engages a player through this effect 2 shadow cards.
-- Each player chooses in turn, after the choices of the players before him.
instance RunMessage AradorsBane where
  runMessage msg t@(AradorsBane attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 0 msg
      pure t
    DoStep n (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      for_ (take 1 $ drop n players) \pid -> do
        enemies <- select (EnemyInStagingArea <> EnemyWithEngagementCostAboveThreatOf pid)
        chooseOrRunOneM pid do
          questionLabeled "aradorsBane.choose"
          causedBy attrs
          targets enemies \e -> do
            engageEnemy pid e
            replicateM_ 2 $ dealShadowCard e
          labeled "aradorsBane.search" $ searchEncounterDeckAndDiscard pid attrs trollOrGiant
        doStep (n + 1) (WhenRevealed $ toTarget attrs)
      pure t
    _ -> AradorsBane <$> runMessage msg attrs
   where
    trollOrGiant = CardMatchAll [CardWithType EnemyType, CardMatchAny [CardWithTrait Troll, CardWithTrait Giant]]
