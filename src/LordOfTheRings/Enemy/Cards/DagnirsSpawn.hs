module LordOfTheRings.Enemy.Cards.DagnirsSpawn (dagnirsSpawn) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Campaign (campaignPartNumber)
import LordOfTheRings.Trait (Trait (Dragon))

newtype DagnirsSpawn = DagnirsSpawn EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Ered Mithrin campaign, from The King's Quest.
dagnirsSpawn :: EnemyCard DagnirsSpawn
dagnirsSpawn = enemy DagnirsSpawn Cards.dagnirsSpawn

-- | Cannot have non-Dragon attachments. Its threat and defense are X, the campaign card's
-- part number.
instance HasModifiersFor DagnirsSpawn where
  getModifiersFor (DagnirsSpawn a) = do
    x <- campaignPartNumber
    modifySelf a $ CannotHaveAttachmentsWithoutTrait Dragon : [m | x > 0, m <- [ThreatModifier x, DefenseModifier x]]

{- | When Revealed: Each player may distribute X damage among characters they control. If
they do, shuffle Dagnir's Spawn into the encounter deck. Read as every player doing so: one
who declines keeps it in play, though the others may still deal theirs.
-}
instance RunMessage DagnirsSpawn where
  runMessage msg e@(DagnirsSpawn attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- campaignPartNumber
      players <- getRemainingPlayers
      let offer :: ReverseQueue n => [PlayerId] -> Bool -> n ()
          offer [] everyone = when everyone $ shuffleIntoEncounterDeck attrs
          offer (p : ps) everyone = chooseOneM p do
            questionLabeledWith "dagnirsSpawn.distribute" [intVar "n" x]
            causedBy attrs
            labeled "dagnirsSpawn.deal" do
              dealDamageDivided attrs p x
              offer ps everyone
            labeled "dagnirsSpawn.decline" $ offer ps False
      offer players True
      pure e
    _ -> DagnirsSpawn <$> runMessage msg attrs
