module LordOfTheRings.Game.Combat (module LordOfTheRings.Game.Combat) where

import Data.Map.Strict qualified as Map (
  fromListWith,
  lookup,
  toList,
 )
import LordOfTheRings.Card (
  CardDef (cdCardType),
  CardType (
    EnemyType,
    EventType,
    LocationType,
    ObjectiveAllyType,
    ObjectiveType,
    TreacheryType
  ),
  EncounterCard,
  HasCardCode (toCardCode),
  HasCardDef (toCardDef),
  IsCard (toCard),
 )
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Classes.HasQueue (HasQueue (pushAll), push)
import LordOfTheRings.Enemy (createEnemy)
import LordOfTheRings.Entities (Entities (entitiesQuests))
import LordOfTheRings.Event (lookupEvent)
import LordOfTheRings.Game.Base (Game (gameEntities))
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Character (
  characterCardDef,
  getHitPoints,
 )
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.I18n (ikeyWith, intVar)
import LordOfTheRings.Id (EnemyId, PlayerId)
import LordOfTheRings.Keyword (Keyword (Phantom, Ranged))
import LordOfTheRings.Location (createLocation)
import LordOfTheRings.Matcher (
  AllyMatcher (
    AllyControlledBy,
    AllyWithId,
    AttackingAlly,
    ReadyAlly
  ),
  EnemyMatcher (AnyEnemy, EnemyEngagedWith, EnemyInStagingArea, EnemyWithId),
  HeroMatcher (AttackingHero, HeroControlledBy, ReadyHero),
  LocationMatcher (IsActiveLocation),
  PlayerMatcher (PlayerWithId),
  Query (select),
  characterController,
  getDamage,
  getEnemy,
  getHero,
  getPlayerOrder,
  hasKeyword,
  hasTrait,
  selectAny,
  selectCharacters,
  setAsideOwner,
  shipEnemy,
  shipObjective,
  shipObjectivesOf,
 )
import LordOfTheRings.Message (
  Message (
    AttackDamage,
    CheckDefeated,
    CheckWindows,
    DamageAflameLocation,
    DamageAnotherCharacter,
    DamageControlledShip,
    DealDamage,
    DeclareAttackers,
    DeclareDefenders,
    DeclareSentinels,
    DefendWith,
    DefendedByMany,
    DiscardTopOfDeck,
    EnemyAttack,
    OptionalActionWindow,
    PlaceTokens,
    PlaceUnderneath,
    RaiseThreat,
    RemoveTokens,
    ResolveShadow,
    UndefendedAttack,
    UndefendedDamage
  ),
  chooseOrRunOne,
  questionLabelWith,
 )
import LordOfTheRings.Modifier (
  ModifierType (
    AttackDamageCanceled,
    AttackDamageToThreatInstead,
    AttackableByAnyCharacter,
    AttackableInStagingAreaByRanged,
    AttackedAsIfEngagedWith,
    AttackersPerAttackLimit,
    AttackersPerRoundLimit,
    AttacksAsIfEngagedWith,
    CanAttackStagingArea,
    CannotAttack,
    CannotAttackMatching,
    CannotAttackOrDefend,
    CannotBeAttacked,
    CannotBeAttackedByCharactersOf,
    CannotBeAttackedByKeyword,
    AlliesCannotBeDeclaredDefenders,
    DefendersControlledBy,
    CannotBeDefendedByAllies,
    CannotDefendAgainst,
    CannotExhaust,
    CannotFight,
    CannotLeaveStagingArea,
    CaptureInsteadOfAttackDamage,
    DeclaredAttackerThisRound,
    DoesNotExhaustToAttack,
    DoesNotExhaustToDefend,
    DoesNotExhaustToDefendAgainst,
    EngagedWithEachPlayer,
    ExcessDamageDiscardsFromDeck,
    ExcessDamageRemovesQuestProgress,
    ExcessDamageTo,
    ExcessDamageToActiveLocation,
    ExcessDamageToAflameLocation,
    ExcessDamageToOtherCharacters,
    ExcessDamageToShipObjective,
    ExcessDamageToShipOneAtATime,
    ExcessDamageToThreat,
    NotFoughtByAllies,
    NotFoughtByDamaged,
    NotFoughtByHeroesWithoutResources,
    OnlyAttackedByShipObjectives,
    CanBeFoughtByShipObjectives,
    OnlyFoughtBy,
    OnlyFoughtByKeyword
  ),
 )
import LordOfTheRings.Objective (createObjective)
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Question (targetLabel)
import LordOfTheRings.Source (
  Source (EnemyAttackSource),
  sourceTarget,
 )
import LordOfTheRings.Target (
  Target (
    AllyTarget,
    EnemyTarget,
    GameTarget,
    HeroTarget,
    LocationTarget,
    PlayerTarget
  ),
  Targetable (toTarget),
 )
import LordOfTheRings.Token (Token (Damage, Progress))
import LordOfTheRings.Treachery (createTreachery)
import LordOfTheRings.Window (
  WindowType (ThreatRaisedByEncounter),
  mkAfter,
 )

import LordOfTheRings.Game.Cards (withKeyword)
import LordOfTheRings.Game.Dispatch (runEach)

seatAfter :: [PlayerId] -> PlayerId -> PlayerId
seatAfter order pid = case dropWhile (/= pid) order of
  (_ : next : _) -> next
  _ -> fromMaybe pid (headMay order)

{- | The characters that may take another point of the damage being assigned: damage
assigned among characters cannot be assigned past a character's remaining hit points
(ALeP 3.088, 3.132, 3.263).
-}
withHitPointsLeft :: [Target] -> [Target] -> GameT [Target]
withHitPointsLeft assigned = filterM \c -> do
  left <- (-) <$> getHitPoints c <*> getDamage c
  pure $ left > length (filter (== c) assigned)

-- | The damage assigned among characters, a point at a time, dealt once all is assigned.
-- | The player assigns the next point of the damage to one of the characters, or, with all
-- assigned or none left to take it, it is dealt.
divideDamage :: Source -> PlayerId -> Int -> [Target] -> [Target] -> (Int -> [Target] -> Message) -> GameT ()
divideDamage source pid n assigned characters next
  | n > 0 && notNull characters =
      push
        $ questionLabelWith (ikeyWith "label.dealDamageDivided" [intVar "n" n]) (sourceTarget source) [] pid
          [targetLabel c [next (n - 1) (c : assigned)] | c <- characters]
  | otherwise = pushAll $ dealAssigned source assigned

dealAssigned :: Source -> [Target] -> [Message]
dealAssigned source assigned = [DealDamage source c k | (c, k) <- Map.toList (Map.fromListWith (+) [(c, 1) | c <- assigned])]

-- | Enemies engaged with a player, highest engagement cost first.
engagedByCost :: PlayerId -> GameT [EnemyId]
engagedByCost pid = byCost =<< select (EnemyEngagedWith $ PlayerWithId pid)

byCost :: [EnemyId] -> GameT [EnemyId]
byCost enemies = do
  costs <- traverse getEngagementCost enemies
  pure $ map fst $ sortOn (Down . snd) $ zip enemies costs

-- | The enemies that attack the player in the combat phase, by engagement cost: those
-- engaged with him, and those in the staging area that attack him as if engaged.
attacksOf :: PlayerId -> GameT [EnemyId]
attacksOf pid = byCost . ordNub =<< (<>) <$> select (EnemyEngagedWith $ PlayerWithId pid) <*> attacksAsIfEngaged pid

-- | The enemies the players may engage from the staging area: those there, not one only
-- considered to be, as Tide-runner engaged or in the Stormcaller's area. Enemies
-- that cannot leave staging must not be selected for another engagement check.
engageableFromStaging :: GameT [EnemyId]
engageableFromStaging = filterM canEngage =<< select EnemyInStagingArea
 where
  canEngage eid = do
    enemy <- getEnemy eid
    mods <- getModifiers eid
    pure $ enemy.placement == StagingArea && CannotLeaveStagingArea `notElem` mods

-- | The enemies in the staging area that attack the player as if engaged, as Snow-Giant.
attacksAsIfEngaged :: PlayerId -> GameT [EnemyId]
attacksAsIfEngaged pid = filterM asIf =<< select EnemyInStagingArea
 where
  asIf e = getModifiers e >>= \ms -> anyM (\m -> selectAny (PlayerWithId pid <> m)) [m | AttacksAsIfEngagedWith m <- ms]

-- | Whether the enemy is engaged with each player from the staging area, as Durin's Bane.
engagedWithEach :: EnemyId -> GameT Bool
engagedWithEach eid = any (\case EngagedWithEachPlayer _ -> True; _ -> False) <$> getModifiers eid

-- | The characters that may attack an enemy in the staging area.
stagingAttackers :: [Target] -> GameT [Target]
stagingAttackers = filterM (fmap (elem CanAttackStagingArea) . getModifiers)

-- | The player's characters that may attack or defend.
readyCharacters :: PlayerId -> GameT [Target]
readyCharacters pid = do
  heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
  allies <- select (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
  filterM (fmap (notElem CannotAttackOrDefend) . getModifiers)
    $ map toTarget heroes
    <> map toTarget allies

-- | The player's characters that may attack: one that cannot exhaust only if it attacks
-- without exhausting.
readyAttackers :: PlayerId -> GameT [Target]
readyAttackers pid = do
  -- a player who cannot declare attacks, as Hobbit-sense reads
  barred <- elem CannotAttack <$> getModifiers (PlayerTarget pid)
  let free ms = CannotExhaust `notElem` ms || DoesNotExhaustToAttack `elem` ms
  if barred then pure [] else filterM (fmap (\ms -> CannotAttack `notElem` ms && free ms) . getModifiers) =<< readyCharacters pid

{- | Shadow cards are not in play, so the card's own module resolves the effect
through a temporary instance of it.
-}
resolveShadowEffect :: EncounterCard -> Message -> GameT ()
resolveShadowEffect card msg = case cdCardType (toCardDef card) of
  EnemyType -> void . runEach msg . createEnemy card =<< getRandom
  LocationType -> void . runEach msg . createLocation card =<< getRandom
  TreacheryType -> void . runEach msg . createTreachery card =<< getRandom
  t | t `elem` [ObjectiveType, ObjectiveAllyType] -> void . runEach msg . createObjective card =<< getRandom
  -- a player event with the Encounter keyword, as Wind from the Sea, built for its owner
  EventType -> setAsideOwner card.id >>= traverse_ \owner -> for_ (lookupEvent (toCardCode card) card.id owner) $ void . runEach msg
  _ -> pure ()

{- | The damage an enemy's attack deals to a character, and, for an enemy whose excess
damage counts against the player, the threat it raises.
-}
attackDamage :: PlayerId -> EnemyId -> Target -> Int -> GameT [Message]
attackDamage pid eid target n = do
  -- all of it canceled, as Bilbo's Magic Ring cancels it
  canceled <- elem (AttackDamageCanceled eid) <$> getModifiers GameTarget
  phantom <- isPhantom eid
  -- an enemy with Phantom deals none: the defending player raises his threat as much
  -- (The Flame of the West rules insert, "Phantom")
  if
    | canceled -> pure []
    | phantom -> pure $ concat [[RaiseThreat pid n, CheckWindows [mkAfter $ ThreatRaisedByEncounter pid n]] | n > 0]
    | otherwise -> attackDamage' pid eid target n

-- | Whether the enemy has Phantom, printed or gained, and not lost.
isPhantom :: EnemyId -> GameT Bool
isPhantom eid = do
  e <- getEnemy eid
  hasKeyword Phantom (toTarget eid) (toCardDef e)

attackDamage' :: PlayerId -> EnemyId -> Target -> Int -> GameT [Message]
attackDamage' pid eid target n = do
  mods <- getModifiers eid
  let excessToThreat = ExcessDamageToThreat `elem` mods
      excessToOthers = ExcessDamageToOtherCharacters `elem` mods
      excessToLocation = ExcessDamageToActiveLocation `elem` mods
      excessToQuests = ExcessDamageRemovesQuestProgress `elem` mods
      excessToShip = ExcessDamageToShipObjective `elem` mods
      -- a card it goes to, as Dense Thickets' shadow sends it to a Ranger objective
      excessTo = [t | ExcessDamageTo t <- mods, t /= target]
      excessToAflame = ExcessDamageToAflameLocation `elem` mods
      excessToDeck = ExcessDamageDiscardsFromDeck `elem` mods
  -- what is dealt past the character's remaining hit points (an attack turned on a
  -- location, as a Battleground takes it, has none)
  let character = case target of
        HeroTarget _ -> True
        AllyTarget _ -> True
        _ -> False
  excess <-
    if character
      then (\remaining -> max 0 (n - remaining)) <$> ((-) <$> getHitPoints target <*> getDamage target)
      else pure 0
  active <- select IsActiveLocation
  quests <- getsGame (toList . entitiesQuests . gameEntities)
  es <- getsGame gameEntities
  ships <- filter (/= target) . map toTarget <$> shipObjectivesOf pid
  let toShip =
        [ chooseOrRunOne pid [targetLabel s [DealDamage (EnemyAttackSource eid) s excess] | s <- ships]
        | excessToShip
        , excess > 0
        , notNull ships
        ]
  -- each point on an Aflame location of the player's choice, so a location the first
  -- points destroy takes no more
  let toAflame = replicate (if excessToAflame then excess else 0) (DamageAflameLocation (EnemyAttackSource eid) pid)
  let captured = case target of
        HeroTarget h -> toCard <$> Map.lookup h es.heroes
        AllyTarget a -> toCard <$> Map.lookup a es.allies
        _ -> Nothing
  defending <- fromMaybe pid <$> characterController target
  pure $ case captured of
    -- it captures the character instead, as Cruel Torturer reads
    Just card | CaptureInsteadOfAttackDamage `elem` mods, n > 0 -> [PlaceUnderneath (EnemyTarget eid) [card]]
    -- or the defending player's threat is raised as much, as The Two Watchers' is
    _ | AttackDamageToThreatInstead `elem` mods -> [x | n > 0, x <- [RaiseThreat defending n, CheckWindows [mkAfter $ ThreatRaisedByEncounter defending n]]]
    _ ->
      DealDamage (EnemyAttackSource eid) target n
        : concat [[RaiseThreat pid excess, CheckWindows [mkAfter $ ThreatRaisedByEncounter pid excess]] | excessToThreat, excess > 0]
        <> replicate (if excessToOthers then excess else 0) (DamageAnotherCharacter (EnemyAttackSource eid) pid target)
        <> concat [[PlaceTokens (EnemyAttackSource eid) (LocationTarget l) Damage excess, CheckDefeated (EnemyAttackSource eid) (LocationTarget l)] | excessToLocation, excess > 0, l <- active]
        <> [RemoveTokens (EnemyAttackSource eid) (toTarget q) Progress (min excess q.progress) | excessToQuests, excess > 0, q <- quests, q.progress > 0]
        <> toShip
        <> replicate (if ExcessDamageToShipOneAtATime `elem` mods then excess else 0) (DamageControlledShip (EnemyAttackSource eid) pid)
        <> [chooseOrRunOne pid [targetLabel t [DealDamage (EnemyAttackSource eid) t excess] | t <- excessTo] | notNull excessTo, excess > 0]
        <> toAflame
        <> [DiscardTopOfDeck pid excess | excessToDeck, excess > 0]

-- | The player's characters that may yet be declared as attackers against the enemy.
ownAttackers :: PlayerId -> EnemyId -> GameT [Target]
ownAttackers pid eid = do
  -- only those allowed to may attack an enemy in the staging area, unless anyone may
  -- one engaged with the player from the staging area, as Durin's Bane, is attacked as engaged
  staged <- selectAny (EnemyWithId eid <> EnemyInStagingArea <> not_ (EnemyEngagedWith $ PlayerWithId pid))
  mods <- getModifiers eid
  -- one attacked as if it were engaged with him, as Charge into Battle's, is attacked as
  -- engaged too
  let open' = AttackableByAnyCharacter `elem` mods || AttackedAsIfEngagedWith pid `elem` mods
      fromStaging cs = (<>) <$> stagingAttackers cs <*> if AttackableInStagingAreaByRanged `elem` mods then withKeyword Ranged cs else pure []
  -- one who does not exhaust to attack is still declared only once
  attacking <- selectCharacters AttackingHero AttackingAlly
  -- none of them, as Umbar Captain bars them from Ship enemies
  let barred = CannotBeAttackedByCharactersOf pid `elem` mods
  filter (\c -> not barred && c `notElem` attacking)
    . ordNub
    <$> ( filterM (canAttack eid)
            =<< (if staged && not open' then fromStaging else pure)
            =<< readyAttackers pid
        )

{- | Who may attack the enemy for the player: their own characters, then the ranged
characters of other players, when it is engaged with them.
-}
mayAttack :: PlayerId -> EnemyId -> GameT [Target]
mayAttack pid eid = do
  blocked <- elem CannotBeAttacked <$> getModifiers eid
  if blocked then pure [] else mayAttack' pid eid

mayAttack' :: PlayerId -> EnemyId -> GameT [Target]
mayAttack' pid eid = do
  own <- ownAttackers pid eid
  engaged <- engagedForAttack pid eid
  mods <- getModifiers eid
  others <- if engaged then filter (\p -> p /= pid && CannotBeAttackedByCharactersOf p `notElem` mods) <$> getPlayerOrder else pure []
  ranged <- filterM (canAttack eid) =<< withKeyword Ranged . concat =<< traverse readyAttackers others
  -- none with a keyword it bars, as Pitch-dark Thicket's shadow bars ranged
  let barred = [k | CannotBeAttackedByKeyword k <- mods]
  filterM (\c -> characterCardDef c >>= \def -> not <$> anyM (\k -> hasKeyword k c def) barred) (own <> ranged)

-- | The enemy is engaged with the player, or attacked as if it were (Charge into Battle).
engagedForAttack :: PlayerId -> EnemyId -> GameT Bool
engagedForAttack pid eid =
  orM [selectAny (EnemyWithId eid <> EnemyEngagedWith (PlayerWithId pid)), elem (AttackedAsIfEngagedWith pid) <$> getModifiers eid]

-- | An action window for each player, in turn order, asked only of those with an action.
actionWindows :: GameT [Message]
actionWindows = map OptionalActionWindow <$> getPlayerOrder

-- | The enemy a step of an enemy's attack, or of an attack on one, is about.
attackingEnemy :: Message -> Maybe EnemyId
attackingEnemy = \case
  EnemyAttack _ eid -> Just eid
  DefendWith _ eid _ -> Just eid
  DeclareDefenders _ eid _ -> Just eid
  DeclareSentinels _ eid _ _ -> Just eid
  DefendedByMany _ eid _ -> Just eid
  UndefendedAttack _ eid -> Just eid
  UndefendedDamage _ eid -> Just eid
  AttackDamage eid _ -> Just eid
  ResolveShadow eid _ _ -> Just eid
  DeclareAttackers _ eid -> Just eid
  _ -> Nothing

{- | Whether the character may attack or defend against the enemy: some enemies only
characters of a trait, or with a keyword, can.
-}
canFight :: EnemyId -> Target -> GameT Bool
canFight eid c = do
  mods <- getModifiers eid
  cmods <- getModifiers c
  def <- characterCardDef c
  andM
    [ -- a Ship objective fights only Ship enemies (Dream-chaser rules)
      orM [not <$> isShipObjective c, selectAny (EnemyWithId eid <> shipEnemy), pure $ CanBeFoughtByShipObjectives `elem` mods]
    , -- and only a Ship objective fights one such as Raider Flagship
      if OnlyAttackedByShipObjectives `elem` mods then isShipObjective c else pure True
    , allM (\(t, k) -> orM [hasTrait t c def, hasKeyword k c def]) [(t, k) | OnlyFoughtBy t k <- mods]
    , allM (\k -> hasKeyword k c def) [k | OnlyFoughtByKeyword k <- mods]
    , if NotFoughtByDamaged `elem` mods then (== 0) <$> getDamage c else pure True
    , pure $ case c of
        AllyTarget _ -> NotFoughtByAllies `notElem` mods
        _ -> True
    , barredBy [m | CannotFight m <- cmods]
    , case c of
        HeroTarget hid | NotFoughtByHeroesWithoutResources `elem` mods -> (> 0) . (.resources) <$> getHero hid
        _ -> pure True
    ]
 where
  barredBy ms = not <$> anyM (\m -> selectAny (EnemyWithId eid <> m)) ms

-- | Whether the character may attack the enemy: as 'canFight', unless it cannot attack it.
canAttack :: EnemyId -> Target -> GameT Bool
canAttack eid c = do
  barred <- (\ms -> anyM (\m -> selectAny (EnemyWithId eid <> m)) [m | CannotAttackMatching m <- ms]) =<< getModifiers c
  full <- (<= 0) . fromMaybe 1 <$> attackerRoom eid
  if barred || full then pure False else canFight eid c

-- | How many more characters may attack the enemy this round or in this attack, when it is
-- limited, as Graxar.
attackerRoom :: EnemyId -> GameT (Maybe Int)
attackerRoom eid = do
  mods <- getModifiers eid
  -- and at a time, as The Black Serpent of The Battle of the Pelennor Fields
  attacking <- length <$> selectCharacters AttackingHero AttackingAlly
  let declared = length [() | DeclaredAttackerThisRound <- mods]
  pure $ minimumMay $ [n - declared | AttackersPerRoundLimit n <- mods] <> [n - attacking | AttackersPerAttackLimit n <- mods]

-- | Whether the character may defend against the enemy's attack: one that cannot exhaust
-- only if it defends without exhausting.
canDefend :: EnemyId -> Target -> GameT Bool
canDefend eid c = do
  globalNoAllies <- anyM (fmap (elem AlliesCannotBeDeclaredDefenders) . getModifiers) =<< select AnyEnemy
  noAllies <- (globalNoAllies ||) . elem CannotBeDefendedByAllies <$> getModifiers eid
  stuck <- elem CannotExhaust <$> getModifiers c
  exhausts <- if stuck then exhaustToDefend c eid else pure False
  let isAlly = case c of
        AllyTarget _ -> True
        _ -> False
  -- only a Ship objective defends against a Ship enemy (Dream-chaser rules)
  shipOnly <- andM [selectAny (EnemyWithId eid <> shipEnemy), not <$> isShipObjective c]
  -- nor one that cannot defend against it, as an ally against Uruk of Mordor
  barred <- (\ms -> anyM (\m -> selectAny (EnemyWithId eid <> m)) [m | CannotDefendAgainst m <- ms]) =<< getModifiers c
  controllerAllowed <- defenderControllerAllowed eid c
  if not controllerAllowed || (noAllies && isAlly) || exhausts || shipOnly || barred then pure False else canFight eid c

-- | Whether the character is a Ship objective (Dream-chaser rules).
isShipObjective :: Target -> GameT Bool
isShipObjective = \case
  AllyTarget aid -> selectAny (AllyWithId aid <> shipObjective)
  _ -> pure False

-- | Whether declaring the character as a defender against the enemy exhausts it.
exhaustToDefend :: Target -> EnemyId -> GameT Bool
exhaustToDefend d eid = do
  mods <- getModifiers d
  enemy <- getEnemy eid
  if DoesNotExhaustToDefend `elem` mods
    then pure False
    else
      not
        <$> anyM
          (\t -> hasTrait t (toTarget eid) (toCardDef enemy))
          [t | DoesNotExhaustToDefendAgainst t <- mods]

-- | An objective attack may grant the declaration step to just one player.
defenderControllerAllowed :: EnemyId -> Target -> GameT Bool
defenderControllerAllowed eid target = do
  mods <- getModifiers eid
  controller <- characterController target
  pure $ all (\pid -> controller == Just pid) [pid | DefendersControlledBy pid <- mods]
