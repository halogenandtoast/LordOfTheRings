module LordOfTheRings.Enemy.Cards.DreadnaughtCaptains (captainOfTheHaven, commanderOfTheDreadnaught, descendantOfCastamir, hydraken, ophidian, varjax) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype DreadnaughtCaptain = DreadnaughtCaptain EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

captainOfTheHaven
  , commanderOfTheDreadnaught
  , descendantOfCastamir
  , hydraken
  , ophidian
  , varjax
    :: EnemyCard DreadnaughtCaptain
captainOfTheHaven = enemy DreadnaughtCaptain Cards.captainOfTheHaven
commanderOfTheDreadnaught = enemy DreadnaughtCaptain Cards.commanderOfTheDreadnaught
descendantOfCastamir = enemy DreadnaughtCaptain Cards.descendantOfCastamir
hydraken = enemy DreadnaughtCaptain Cards.hydraken
ophidian = enemy DreadnaughtCaptain Cards.ophidian
varjax = enemy DreadnaughtCaptain Cards.varjax

instance HasModifiersFor DreadnaughtCaptain where
  getModifiersFor (DreadnaughtCaptain a) = do
    modifySelf a
      $ ImmuneToPlayerCardEffects
      : [AttackersPerRoundLimit 1 | toCardCode a == toCardCode Cards.ophidian]
        <> [ExcessDamageToShipOneAtATime | toCardCode a == toCardCode Cards.varjax]
    case a.placement of
      EngagedWith pid -> do
        bosses <-
          select $ EnemyWithTitle "Dreadnaught"
        for_ bosses \boss -> modified a boss [CannotBeAttackedByCharactersOf pid]
      _ -> pure ()

instance HasAbilities DreadnaughtCaptain where
  getAbilities (DreadnaughtCaptain a)
    | code == toCardCode Cards.captainOfTheHaven =
        [ restricted a 1 (exists $ EnemyWithId a.id <> EnemyEngagedWith (NotPlayer $ PlayerWithCardsInDeck 1))
            $ forced
            $ PhaseEndsWindow When CombatPhase
        ]
    | code == toCardCode Cards.commanderOfTheDreadnaught =
        [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]
    | code == toCardCode Cards.descendantOfCastamir =
        [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]
    | code == toCardCode Cards.hydraken =
        [limited (GroupLimit PerRound 1) $ mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]
    | code == toCardCode Cards.ophidian =
        [mkAbility a 1 $ forced $ DefenderDeclaredAgainstWindow After a.id]
    | otherwise = []
   where
    code = toCardCode a

instance RunMessage DreadnaughtCaptain where
  runMessage msg e@(DreadnaughtCaptain attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      if
        | toCardCode attrs == toCardCode Cards.captainOfTheHaven ->
            for_ [pid | EngagedWith pid <- [attrs.placement]] eliminatePlayer
        | toCardCode attrs == toCardCode Cards.commanderOfTheDreadnaught -> do
            bosses <-
              select $ EnemyWithTitle "Dreadnaught"
            for_ [pid | Window _ (EnemyAttacking eid pid) <- ws, eid == attrs.id] \pid ->
              traverse_ (immediateAttack pid) bosses
        | toCardCode attrs == toCardCode Cards.descendantOfCastamir ->
            for_ [pid | Window _ (EnemyEngaged eid pid) <- ws, eid == attrs.id] \pid ->
              replicateM_ 2 $ push $ RevealFromCorsairDeckWithShadow (toSource attrs) pid
        | toCardCode attrs == toCardCode Cards.hydraken -> do
            for_ [pid | Window _ (EnemyAttacking eid pid) <- ws, eid == attrs.id] \pid ->
              immediateAttackWithPreparation 3 pid attrs.id
                $ for_ [engaged | EngagedWith engaged <- [attrs.placement]] \engaged -> encounterRaisesThreat engaged 4
        | otherwise -> for_ [c | Window _ (DeclaredAsDefender c eid) <- ws, eid == attrs.id] \c -> dealDamage attrs c 2
      pure e
    _ -> DreadnaughtCaptain <$> runMessage msg attrs
