module LordOfTheRings.Scenario.Scenarios.JourneyInTheDark.Weapons (
  balrogWeaponModifiers,
  runBalrogWeapon,
) where

import LordOfTheRings.Card (Card (EncounterCard), toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Scenario.Scenarios.JourneyInTheDark.Helpers (getBalrog)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Trait (Trait (Weapon))
import LordOfTheRings.Treachery.Import.Lifted

-- | A weapon can attach to the set-aside card before an EnemyId exists. Resolve
-- that card's host immediately when its enemy enters, before any entry responses.
weaponHost :: HasGame m => Target -> m Target
weaponHost host@(CardIdTarget cid) = do
  enemy <- getsGame (find ((== cid) . (.cardId) . toAttrs) . toList . (.entities.enemies))
  pure $ maybe host toTarget enemy
weaponHost host = pure host

balrogWeaponModifiers :: HasModifiersM m => TreacheryAttrs -> [ModifierType] -> m ()
balrogWeaponModifiers a mods = withHost a \host -> do
  resolved <- weaponHost host
  modifySelf a [GainTrait Weapon]
  case resolved of
    CardIdTarget _ -> modifySelf a [ConsideredOutOfPlay]
    EnemyTarget _ -> modified a resolved mods
    _ -> pure ()

attachToBalrog :: ReverseQueue m => TreacheryAttrs -> m ()
attachToBalrog a = getBalrog >>= \case
  Just eid -> attachTreachery a.id (EnemyTarget eid)
  Nothing -> do
    cards <- getsGame \g ->
      let s = toAttrs g.scenario
       in map EncounterCard (scenarioSetAside s <> scenarioEncounterDiscard s <> scenarioEncounterDeck s) <> scenarioVictoryDisplay s
    for_ (find ((== toCardCode Enemies.theBalrog) . toCardCode) cards) \card ->
      attachTreachery a.id (CardIdTarget card.id)

runBalrogWeapon :: Message -> TreacheryAttrs -> QueueT Message GameT TreacheryAttrs
runBalrogWeapon msg a = case msg of
  WhenRevealed (isTarget a -> True) -> a <$ attachToBalrog a
  ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
    whenM (selectAny $ EnemyWithId eid <> EnemyWithCardCode (toCardCode Enemies.theBalrog)) do
      push $ PutShadowTreacheryIntoPlay eid cid (AttachedTo $ EnemyTarget eid)
    pure a
  CheckWindows _ | AttachedTo host@(CardIdTarget _) <- a.placement -> do
    resolved <- weaponHost host
    when (resolved /= host) $ attachTreachery a.id resolved
    runMessage msg a
  _ -> runMessage msg a
