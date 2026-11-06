module LordOfTheRings.Enemy.Cards.Dreadnaught (dreadnaught, dreadnaughtDamaged) where

import LordOfTheRings.Card (cdUnique, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Epic.Types (EpicReplica (epicShared), SharedKey (DreadnaughtDamage), sharedCounter)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Scenario.Scenarios.TheHuntForTheDreadnaught.Helpers
import LordOfTheRings.Scenario.Types (scenarioSetAside, scenarioEpic)
import LordOfTheRings.Trait (Trait (Corsair, Ship))

newtype Dreadnaught = Dreadnaught EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

dreadnaught, dreadnaughtDamaged :: EnemyCard Dreadnaught
dreadnaught = enemy Dreadnaught Cards.dreadnaught
dreadnaughtDamaged = enemy Dreadnaught Cards.dreadnaughtDamaged

isDamaged :: EnemyAttrs -> Bool
isDamaged = (== toCardCode Cards.dreadnaughtDamaged) . toCardCode

instance HasModifiersFor Dreadnaught where
  getModifiersFor (Dreadnaught a) = do
    groupSize <- initialGroupSize
    total <- globalPlayerCount
    modifySelf a
      $ [ImmuneToPlayerCardEffects, HitPointModifier (15 * total), DamageLimitEachRound (5 * groupSize)]
        <> [GainKeyword (Archery (groupSize - 1)) | not (isDamaged a), groupSize > 1]

instance HasAbilities Dreadnaught where
  getAbilities (Dreadnaught a) =
    [mkAbility a 3 $ silent $ MatchingEnemyEntersPlayWindow After (EnemyWithId a.id)]
      <> if isDamaged a
        then [mkAbility a 2 $ forced $ FlippedToWindow After (toCardCode Cards.dreadnaughtDamaged)]
        else [mkAbility a 1 $ silent RoundEndsWindow]

instance RunMessage Dreadnaught where
  runMessage msg e@(Dreadnaught attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 3 _ -> do
      replica <- getsGame (scenarioEpic . toAttrs . gameScenario)
      for_ replica $ push . SyncEpic . epicShared
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      hp <- (15 *) <$> globalPlayerCount
      when (2 * attrs.damage > hp) $ flipOver attrs
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      eachPlayer \pid -> push $ WithTarget (PlayerTarget pid) msg
      pure e
    WithTarget (PlayerTarget pid) (UseCardAbility _ (isSource attrs -> True) 2 _) -> do
      cards <- getsGame (scenarioSetAside . toAttrs . gameScenario)
      shuffled <-
        shuffleM
          [ card
          | card <- cards
          , cdUnique (toCardDef card)
          , cardMatches (CardWithTrait Corsair <> NotCard (CardWithTrait Ship)) card
          ]
      for_ (take 1 shuffled) \card -> push $ PutEnemyIntoPlayEngaged pid card
      pure e
    PlaceTokens _ (isTarget attrs -> True) Damage _ -> do
      changed <- runMessage msg attrs
      let added = changed.damage - attrs.damage
      when (added > 0) $ push $ ContributeShared DreadnaughtDamage added
      pure $ Dreadnaught changed
    SyncEpic shared -> do
      let damage = sharedCounter DreadnaughtDamage shared
      push $ CheckDefeated GameSource (toTarget attrs)
      pure $ Dreadnaught $ attrs & tokensL %~ (addTokens Damage damage . removeAllTokens Damage)
    _ -> Dreadnaught <$> runMessage msg attrs
