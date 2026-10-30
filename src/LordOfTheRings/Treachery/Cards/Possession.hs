module LordOfTheRings.Treachery.Cards.Possession (possession) where

import LordOfTheRings.Ally.Types (allyCardId)
import LordOfTheRings.Card (CardCost (StaticCost), CardDef (cdCost), toCardDef)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Possession = Possession TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

possession :: TreacheryCard Possession
possession = treachery Possession Cards.possession

{- | Doomed 1.
When Revealed: Destroy the highest cost ally in play. That ally's controller reanimates
that character, adds it to the staging area, and attaches Possession to it. (Counts as a
Condition attachment with the text: "If Possession is discarded, the attached card is
also discarded.")
With a tie the first player chooses which; objective allies have no cost.
-}
instance RunMessage Possession where
  runMessage msg t@(Possession attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      allies <- traverse (\aid -> (aid,) <$> getAlly aid) =<< select (not_ ObjectiveAlly)
      let cost a = case cdCost (toCardDef a) of
            Just (StaticCost n) -> n
            _ -> 0
          highest = foldr (max . cost . snd) 0 allies
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "possession.destroy"
        causedBy attrs
        targets [aid | (aid, a) <- allies, cost a == highest] \aid -> do
          card <- allyCardId . toAttrs <$> getAlly aid
          destroy attrs aid
          eid <- reanimate card
          whileInPlay (EnemyTarget eid) $ attachTreachery attrs.treacheryId (EnemyTarget eid)
      pure t
    Discard (TreacheryTarget tid) | tid == attrs.id, AttachedTo host <- attrs.placement -> do
      whileInPlay host $ discard host
      Possession <$> runMessage msg attrs
    _ -> Possession <$> runMessage msg attrs
