module LordOfTheRings.Objective.Cards.LakeTown (lakeTown) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Helpers.Character (getHitPoints)
import LordOfTheRings.Helpers.Window (exhaustableCharacters)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))

newtype LakeTown = LakeTown ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lakeTown :: ObjectiveCard LakeTown
lakeTown = objective LakeTown Cards.lakeTown

instance RunMessage LakeTown where
  runMessage msg o@(LakeTown a) = case msg of
    PlaceTokens _ (isTarget a -> True) Damage _ -> do
      a' <- runMessage msg a
      when (countTokens Damage (objectiveTokens a') >= 50) $ gameOver Lost
      pure $ LakeTown a'
    ResolveBurnDamage source -> do
      locations <- traverse getLocation =<< select AnyLocation
      damage <- sum <$> traverse (\l -> getBurn (toTarget l) (toCardDef l)) locations
      players <- getPlayerOrder
      push $ AssignBurnDamage source damage [] players
      pure o
    -- The Burn X rule lets players exhaust characters once, then assign up to
    -- their remaining hit points. Deal the damage only after assignment ends.
    AssignBurnDamage source n assigned players -> do
      case (n, players) of
        (remaining, pid : rest) | remaining > 0 -> do
          available <- exhaustableCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
          controlled <- selectCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
          let already = filter (`elem` assigned) controlled
          candidates <- filterM (hasCapacity assigned) $ nub (available <> already)
          if null candidates
            then push $ AssignBurnDamage source n assigned rest
            else chooseOneM pid do
              questionLabeledWith "lakeTown.assignBurn" [intVar "n" n]
              causedBy a
              targets candidates \target -> do
                unless (target `elem` assigned) $ exhaust target
                push $ AssignBurnDamage source (n - 1) (target : assigned) players
              labeled "lakeTown.takeBurn" $ push $ AssignBurnDamage source n assigned rest
        _ -> do
          for_ (Map.toList $ Map.fromListWith (+) [(target, 1 :: Int) | target <- assigned]) \(target, amount) ->
            dealDamage source target amount
          when (n > 0) $ dealDamage source a n
      pure o
    _ -> LakeTown <$> runMessage msg a
   where
    hasCapacity assigned target = do
      hp <- getHitPoints target
      damage <- getDamage target
      pure $ hp - damage > length (filter (== target) assigned)
