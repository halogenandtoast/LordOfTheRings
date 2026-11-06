module LordOfTheRings.Treachery.Cards.DireMagnificence (direMagnificence) where

import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (smaugEnemies, lakeTownObjectives)

newtype DireMagnificence = DireMagnificence TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

direMagnificence :: TreacheryCard DireMagnificence
direMagnificence = treachery DireMagnificence Cards.direMagnificence

instance RunMessage DireMagnificence where
  runMessage msg t@(DireMagnificence a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      towns <- lakeTownObjectives
      eachPlayer \pid -> do
        allies <- filterM discardable =<< select (AllyControlledBy $ PlayerWithId pid)
        if null allies
          then for_ towns \oid -> dealDamage a oid 2
          else chooseOrRunOneM pid do
            questionLabeled "lakeTown.discardAlly"
            causedBy a
            targets allies $ discard a
      pure t
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> t <$ ifThisAttackDestroys eid msg
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == a.cardId -> do
      players <- getPlayerOrder
      let next = fromMaybe pid $ headMay (drop 1 $ dropWhile (/= pid) players) <|> headMay players
      enemies <- smaugEnemies
      for_ enemies \eid -> do
        placeEnemy eid (EngagedWith next)
        immediateAttack next eid
      pure t
    _ -> DireMagnificence <$> runMessage msg a
   where
    discardable aid = do
      mods <- getModifiers aid
      immune <- immuneToSource (toSource a) aid
      pure $ not immune && CannotLeavePlay `notElem` mods && CannotBeDiscardedByEncounterCards `notElem` mods
