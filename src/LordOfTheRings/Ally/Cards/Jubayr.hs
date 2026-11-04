module LordOfTheRings.Ally.Cards.Jubayr (jubayr) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Enemy.Types (enemyShadowRevealed)

newtype Jubayr = Jubayr AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel, as its card def says.
jubayr :: AllyCard Jubayr
jubayr = ally Jubayr Cards.jubayr

-- | Response: After Jubayr exhausts to defend an attack, discard 1 facedown shadow card
-- from a non-unique enemy. (Limit once per phase.) He is exhausted once declared only if
-- he exhausted to defend.
instance HasAbilities Jubayr where
  getAbilities (Jubayr a) =
    [ limited (GroupLimit PerPhase 1)
        $ restricted a 1 (exists (AllyWithId a.id <> ExhaustedAlly) <> exists (not_ UniqueEnemy <> EnemyWithFacedownShadow))
        $ response
        $ DefenderDeclaredWindow After (Just $ toTarget a)
    ]

instance RunMessage Jubayr where
  runMessage msg s@(Jubayr a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- traverse getEnemy =<< select (not_ UniqueEnemy <> EnemyWithFacedownShadow)
      shadowed <- for enemies \e -> (e,) <$> shadowsPlayerCardsCanAffect e
      chooseOneM pid do
        questionLabeled "jubayr.discardShadow"
        causedBy a
        for_ shadowed \(e, shadows) ->
          targets [c | c <- shadows, c.id `notElem` enemyShadowRevealed (toAttrs e)] \c ->
            discardShadowCard e.id c
      pure s
    _ -> Jubayr <$> runMessage msg a
